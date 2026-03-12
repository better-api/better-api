use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::{self, AstNode};

use crate::oracle::{Context, symbols};

/// Check if a given value matches the expected type.
///
/// If value does not match the type, appropriate reports are added to `reports` and
/// `false` is returned. If it matches, `true` is returned.
///
/// Caller does not need to handle reporting for mismatched types, as it is done here.
pub(crate) fn value_matches_type(ctx: &mut Context, val: &ast::Value, typ: &ast::Type) -> bool {
    match (val, typ) {
        (ast::Value::Integer(n), ast::Type::TypeI32(_)) => {
            check_integer::<i32>(n, "i32", ctx.reports)
        }
        (ast::Value::Integer(n), ast::Type::TypeI64(_)) => {
            check_integer::<i64>(n, "i64", ctx.reports)
        }
        (ast::Value::Integer(n), ast::Type::TypeU32(_)) => {
            check_integer::<u32>(n, "u32", ctx.reports)
        }
        (ast::Value::Integer(n), ast::Type::TypeU64(_)) => {
            check_integer::<u64>(n, "u64", ctx.reports)
        }
        (ast::Value::Integer(n), ast::Type::Enum(enm)) => compare_int_enum(n, enm, ctx.reports),

        (ast::Value::Float(_), ast::Type::TypeF32(_)) => true,
        (ast::Value::Float(_), ast::Type::TypeF64(_)) => true,

        (ast::Value::Bool(_), ast::Type::TypeBool(_)) => true,

        (ast::Value::String(_), ast::Type::TypeString(_)) => true,
        (ast::Value::String(_), ast::Type::TypeDate(_)) => todo!(),
        (ast::Value::String(_), ast::Type::TypeTimestamp(_)) => todo!(),
        (ast::Value::String(string), ast::Type::Enum(enm)) => {
            compare_string_enum(string, enm, ctx.reports)
        }

        (ast::Value::Array(arr), ast::Type::TypeArray(arr_type)) => {
            compare_arrays(ctx, arr, arr_type)
        }
        (ast::Value::Object(_), ast::Type::Record(_)) => todo!(),
        (ast::Value::Object(_), ast::Type::Union(_)) => todo!(),
        (ast::Value::Object(_), ast::Type::TypeResponse(_)) => todo!(),

        (ast::Value::Null(_), ast::Type::TypeOption(_)) => true,
        (_, ast::Type::TypeOption(opt)) => match opt.typ() {
            Some(opt) => value_matches_type(ctx, val, &opt),
            // Error is reported by parser.
            None => false,
        },

        (_, ast::Type::TypeRef(reference)) => {
            match symbols::deref(ctx.strings, ctx.symbol_map, ctx.root, reference) {
                Some(typ) => value_matches_type(ctx, val, &typ),
                None => false,
            }
        }

        _ => {
            ctx.reports.push(
                Report::error(format!("expected {typ}, found {val}"))
                    .add_label(Label::primary(
                        format!("expected {typ}, found {val}"),
                        val.syntax().text_range().into(),
                    ))
                    .add_label(Label::secondary(
                        "type defined here".to_string(),
                        typ.syntax().text_range().into(),
                    )),
            );
            false
        }
    }
}

fn check_integer<I: TryFrom<i128>>(
    n: &ast::Integer,
    name: &str,
    reports: &mut Vec<Report>,
) -> bool {
    if I::try_from(n.integer()).is_ok() {
        true
    } else {
        reports.push(
            Report::error(format!("integer value out of range for {name} type")).add_label(
                Label::primary(
                    format!("integer value out of range for {name} type"),
                    n.syntax().text_range().into(),
                ),
            ),
        );
        false
    }
}

fn compare_int_enum(n: &ast::Integer, enm: &ast::Enum, reports: &mut Vec<Report>) -> bool {
    let equal = enm.members().filter_map(|mem| mem.value()).any(|mem| {
        let ast::Value::Integer(mem_int) = mem else {
            return false;
        };

        n.integer() == mem_int.integer()
    });

    if !equal {
        reports.push(
            Report::error("integer is not a valid enum member".to_string())
                .add_label(Label::primary(
                    "integer is not a valid enum member".to_string(),
                    n.syntax().text_range().into(),
                ))
                .add_label(Label::secondary(
                    "enum defined here".to_string(),
                    enm.syntax().text_range().into(),
                ))
                .with_note("help: integer has to be equal to one of the enum members".to_string()),
        );
    }

    equal
}

fn compare_string_enum(string: &ast::String, enm: &ast::Enum, reports: &mut Vec<Report>) -> bool {
    let equal = enm.members().filter_map(|mem| mem.value()).any(|mem| {
        let ast::Value::String(mem_string) = mem else {
            return false;
        };

        // AST strings don't have to be escaped before comparison, because escape
        // characters are unique. None of the escaped characters can be created in two
        // different ways. Ie to have a string that contains '\', you need to provide "\\",
        // no other way to create it. So we can compare raw string containing escapes.
        string.string().text() == mem_string.string().text()
    });

    if !equal {
        reports.push(
            Report::error("string is not a valid enum member".to_string())
                .add_label(Label::primary(
                    "string is not a valid enum member".to_string(),
                    string.syntax().text_range().into(),
                ))
                .add_label(Label::secondary(
                    "enum defined here".to_string(),
                    enm.syntax().text_range().into(),
                ))
                .with_note("help: string has to be equal to one of the enum members".to_string()),
        );
    }

    equal
}

fn compare_arrays(ctx: &mut Context, arr: &ast::Array, arr_type: &ast::TypeArray) -> bool {
    let Some(typ) = arr_type.typ() else {
        return false;
    };

    arr.values().all(|val| value_matches_type(ctx, &val, &typ))
}

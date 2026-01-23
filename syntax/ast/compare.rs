use better_api_diagnostic::{Label, Report};

use super::*;

/// Check if a given value matches the expected type.
///
/// If value does not match the type, appropriate reports are added to `reports` and
/// `false` is returned. If it matches, `true` is returned.
///
/// Caller does not need to handle reporting for mismatched types, as it is done here.
pub fn value_matches_type<D: Fn(&TypeRef) -> Option<Type>>(
    val: &Value,
    typ: &Type,
    reports: &mut Vec<Report>,
    deref: D,
) -> bool {
    match (val, typ) {
        (Value::Integer(n), Type::TypeI32(_)) => check_integer::<i32>(n, "i32", reports),
        (Value::Integer(n), Type::TypeI64(_)) => check_integer::<i64>(n, "i64", reports),
        (Value::Integer(n), Type::TypeU32(_)) => check_integer::<u32>(n, "u32", reports),
        (Value::Integer(n), Type::TypeU64(_)) => check_integer::<u64>(n, "u64", reports),
        (Value::Integer(n), Type::Enum(enm)) => compare_int_enum(n, enm, reports),

        (Value::Float(_), Type::TypeF32(_)) => true,
        (Value::Float(_), Type::TypeF64(_)) => true,

        (Value::Bool(_), Type::TypeBool(_)) => true,

        (Value::String(_), Type::TypeString(_)) => true,
        (Value::String(_), Type::TypeDate(_)) => todo!(),
        (Value::String(_), Type::TypeTimestamp(_)) => todo!(),
        (Value::String(string), Type::Enum(enm)) => compare_string_enum(string, enm, reports),

        (Value::Array(arr), Type::TypeArray(arr_type)) => {
            compare_arrays(arr, arr_type, reports, deref)
        }
        (Value::Object(_), Type::Record(_)) => todo!(),
        (Value::Object(_), Type::Union(_)) => todo!(),
        (Value::Object(_), Type::TypeResponse(_)) => todo!(),

        // TODO: handle `null` separately, when we get it parsed.
        (_, Type::TypeOption(opt)) => match opt.typ() {
            Some(opt) => value_matches_type(val, &opt, reports, deref),
            // Error is reported by parser.
            None => false,
        },

        (_, Type::TypeRef(reference)) => match deref(reference) {
            Some(typ) => value_matches_type(val, &typ, reports, deref),
            None => false,
        },

        _ => {
            reports.push(
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

fn check_integer<I: TryFrom<i128>>(n: &Integer, name: &str, reports: &mut Vec<Report>) -> bool {
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

fn compare_int_enum(n: &Integer, enm: &Enum, reports: &mut Vec<Report>) -> bool {
    let equal = enm.members().filter_map(|mem| mem.value()).any(|mem| {
        let Value::Integer(mem_int) = mem else {
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

fn compare_string_enum(string: &String, enm: &Enum, reports: &mut Vec<Report>) -> bool {
    let equal = enm.members().filter_map(|mem| mem.value()).any(|mem| {
        let Value::String(mem_string) = mem else {
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

fn compare_arrays<D: Fn(&TypeRef) -> Option<Type>>(
    arr: &Array,
    arr_type: &TypeArray,
    reports: &mut Vec<Report>,
    deref: D,
) -> bool {
    let Some(typ) = arr_type.typ() else {
        return false;
    };

    arr.values()
        .all(|val| value_matches_type(&val, &typ, reports, &deref))
}

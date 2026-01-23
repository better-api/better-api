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
        (Value::Integer(_), Type::Enum(_)) => todo!(),

        (Value::Float(_), Type::TypeF32(_)) => true,
        (Value::Float(_), Type::TypeF64(_)) => true,

        (Value::Bool(_), Type::TypeBool(_)) => true,

        (Value::String(_), Type::TypeString(_)) => true,
        (Value::String(_), Type::TypeDate(_)) => todo!(),
        (Value::String(_), Type::TypeTimestamp(_)) => todo!(),
        (Value::String(_), Type::Enum(_)) => todo!(),

        (Value::Array(_), Type::TypeArray(_)) => todo!(),
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
                Report::error(format!("expected {typ}, found {val}")).add_label(Label::primary(
                    format!("expected {typ}, found {val}"),
                    val.syntax().text_range().into(),
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

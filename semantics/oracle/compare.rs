use std::collections::HashMap;

use better_api_diagnostic::{Label, Report};
use better_api_syntax::TextRange;
use better_api_syntax::ast::{self, AstNode};

use time::format_description::well_known::Rfc3339;
use time::macros::format_description;
use time::{Date, OffsetDateTime};

use crate::oracle::{Context, symbols};
use crate::string::StringId;
use crate::text::{lower_name, parse_string};

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
        (ast::Value::String(string), ast::Type::TypeDate(_)) => compare_date(ctx, string),
        (ast::Value::String(string), ast::Type::TypeTimestamp(_)) => compare_timestamp(ctx, string),
        (ast::Value::String(string), ast::Type::Enum(enm)) => {
            compare_string_enum(string, enm, ctx.reports)
        }

        (ast::Value::Array(arr), ast::Type::TypeArray(arr_type)) => {
            compare_arrays(ctx, arr, arr_type)
        }
        (ast::Value::Object(obj), ast::Type::Record(rec)) => compare_object(ctx, obj, rec),
        (ast::Value::Object(obj), ast::Type::Union(union)) => compare_union(ctx, obj, union),
        (ast::Value::Object(obj), ast::Type::TypeResponse(resp)) => {
            compare_response(ctx, obj, resp)
        }

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

fn compare_date(ctx: &mut Context, val: &ast::String) -> bool {
    const DATE_FORMAT: &[time::format_description::FormatItem<'static>] =
        format_description!("[year]-[month]-[day]");

    let token = val.string();
    let string = parse_string(&token, None);

    let is_valid = Date::parse(&string, DATE_FORMAT).is_ok();
    if !is_valid {
        ctx.reports.push(
            Report::error("invalid date".to_string())
                .add_label(Label::primary(
                    "invalid date".to_string(),
                    val.syntax().text_range().into(),
                ))
                .with_note("help: date has to be in `YYYY-MM-DD` format".to_string()),
        );
    }

    is_valid
}

fn compare_timestamp(ctx: &mut Context, val: &ast::String) -> bool {
    let token = val.string();
    let string = parse_string(&token, None);

    let is_valid = OffsetDateTime::parse(&string, &Rfc3339).is_ok();
    if !is_valid {
        ctx.reports.push(
            Report::error("invalid timestamp".to_string())
                .add_label(Label::primary(
                    "invalid timestamp".to_string(),
                    val.syntax().text_range().into(),
                ))
                .with_note("help: timestamp has to be in RFC3339 format".to_string()),
        );
    }

    is_valid
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

fn compare_object(ctx: &mut Context, obj: &ast::Object, rec: &ast::Record) -> bool {
    struct FieldData {
        field_range: TextRange,
        typ: ast::Type,
        has_default: bool,
        in_obj: bool,
    }
    let mut rec_fields: HashMap<StringId, FieldData> = HashMap::new();
    let mut valid = true;

    // Insert record fields
    for field in rec.fields() {
        let Some(name) = field.name() else {
            continue;
        };

        let Some(typ) = field.typ() else {
            continue;
        };

        let has_default = field.prologue().and_then(|p| p.default()).is_some();

        let Some(name_id) = lower_name(&name, ctx.strings, None) else {
            continue;
        };

        rec_fields.entry(name_id).or_insert(FieldData {
            field_range: field.syntax().text_range(),
            typ,
            has_default,
            in_obj: false,
        });
    }

    // Compare object fields with record
    for field in obj.fields() {
        let Some(name) = field.name() else {
            continue;
        };

        let Some(val) = field.value() else {
            continue;
        };

        let Some(name_id) = lower_name(&name, ctx.strings, None) else {
            continue;
        };

        match rec_fields.get_mut(&name_id) {
            Some(rec_field) => {
                rec_field.in_obj = true;

                let matches = value_matches_type(ctx, &val, &rec_field.typ);
                valid &= matches;
            }
            None => {
                valid = false;

                let name_str = ctx.strings.resolve(name_id);
                ctx.reports.push(
                    Report::error(format!("record has no field `{name_str}`")).add_label(
                        Label::primary(
                            "no such field".to_string(),
                            field.syntax().text_range().into(),
                        ),
                    ),
                );
            }
        }
    }

    // Check all record fields exist in object
    for (name_id, field) in rec_fields
        .iter()
        .filter(|(_, f)| !f.in_obj && !f.has_default)
    {
        valid = false;

        let name_str = ctx.strings.resolve(*name_id);
        ctx.reports.push(
            Report::error(format!("missing record field `{name_str}`"))
                .add_label(Label::primary(
                    format!("missing record field `{name_str}`"),
                    obj.syntax().text_range().into(),
                ))
                .add_label(Label::secondary(
                    "field defined here".to_string(),
                    field.field_range.into(),
                )),
        );
    }

    valid
}

fn compare_union(ctx: &mut Context, obj: &ast::Object, union: &ast::Union) -> bool {
    let help_str = "help: object representing an union must contain only `type` and `data` fields";

    // Find type and data fields.
    let mut type_field = None;
    let mut data_field = None;

    for field in obj.fields() {
        let Some(name) = field.name() else {
            continue;
        };

        let Some(name_id) = lower_name(&name, ctx.strings, None) else {
            continue;
        };

        // Repeated fields don't have to be handled here, they are handled during lowering.
        match ctx.strings.resolve(name_id) {
            "type" => type_field = Some(field),
            "data" => data_field = Some(field),
            name_str => ctx.reports.push(
                Report::error(format!("invalid union field `{name_str}`"))
                    .add_label(Label::primary(
                        "invalid union field".to_string(),
                        field.syntax().text_range().into(),
                    ))
                    .with_note(help_str.to_string()),
            ),
        }
    }

    // Validate both fields are defined
    if type_field.is_none() {
        ctx.reports.push(
            Report::error("missing `type` field".to_string())
                .add_label(Label::primary(
                    "missing `type` field".to_string(),
                    obj.syntax().text_range().into(),
                ))
                .with_note(help_str.to_string()),
        );
    }

    if data_field.is_none() {
        ctx.reports.push(
            Report::error("missing `data` field".to_string())
                .add_label(Label::primary(
                    "missing `data` field".to_string(),
                    obj.syntax().text_range().into(),
                ))
                .with_note(help_str.to_string()),
        );
    }

    let (Some(type_field), Some(data_field)) = (type_field, data_field) else {
        // Errors were already reported, we just exit here
        return false;
    };

    // Get name of option in union
    let Some(ast::Value::String(type_value)) = type_field.value() else {
        let range = type_field.value().map_or_else(
            || type_field.syntax().text_range(),
            |v| v.syntax().text_range(),
        );

        ctx.reports.push(
            Report::error("union `type` field must be a string naming a union option".to_string())
                .add_label(Label::primary(
                    "`type` must be a string".to_string(),
                    range.into(),
                ))
                .with_note("help: set `type` to one of the union option names".to_string()),
        );
        return false;
    };

    let type_value_token = type_value.string();
    let type_value_str = parse_string(&type_value_token, None);

    // Find the type in union
    let union_field = union.fields().find(|field| {
        let Some(name_token) = field.name() else {
            return false;
        };

        let Some(name_id) = lower_name(&name_token, ctx.strings, None) else {
            return false;
        };

        let name_str = ctx.strings.resolve(name_id);
        name_str == type_value_str
    });

    let Some(union_field) = union_field else {
        ctx.reports.push(
            Report::error(format!("`{type_value_str}` is not a valid union option"))
                .add_label(Label::primary(
                    "unknown union option".to_string(),
                    type_value.syntax().text_range().into(),
                ))
                .add_label(Label::secondary(
                    "union defined here".to_string(),
                    union.syntax().text_range().into(),
                ))
                .with_note("help: set `type` to one of the union option names".to_string()),
        );
        return false;
    };

    // Do final comparison
    let Some(value) = data_field.value() else {
        return false;
    };

    let Some(typ) = union_field.typ() else {
        return false;
    };

    value_matches_type(ctx, &value, &typ)
}

fn compare_response(ctx: &mut Context, obj: &ast::Object, resp: &ast::TypeResponse) -> bool {
    let help_str = "help: object representing a response can contain only `headers`, `contentType` and `body` fields";

    let mut is_valid = true;

    // Get fields
    let mut headers_field = None;
    let mut body_field = None;
    let mut content_type_field = None;

    for field in obj.fields() {
        let Some(name) = field.name() else {
            continue;
        };

        let Some(name_id) = lower_name(&name, ctx.strings, None) else {
            continue;
        };

        match ctx.strings.resolve(name_id) {
            "headers" => headers_field = Some(field),
            "contentType" => content_type_field = Some(field),
            "body" => body_field = Some(field),

            name_str => ctx.reports.push(
                Report::error(format!("invalid response field `{name_str}`"))
                    .add_label(Label::primary(
                        "invalid response field".to_string(),
                        field.syntax().text_range().into(),
                    ))
                    .with_note(help_str.to_string()),
            ),
        }
    }

    // Check content type
    if !is_content_type_valid(ctx, content_type_field.and_then(|f| f.value())) {
        is_valid = false;
    }

    // Check headers.
    match (
        headers_field.and_then(|f| f.value()),
        resp.headers().and_then(|h| h.typ()),
    ) {
        (None, None) => (),
        (Some(headers_val), None) => {
            is_valid = false;

            ctx.reports.push(
                Report::error("response type has no `headers` field".to_string())
                    .add_label(Label::primary(
                        "unexpected `headers` field".to_string(),
                        headers_val.syntax().text_range().into(),
                    ))
                    .add_label(Label::secondary(
                        "response type defined here".to_string(),
                        resp.syntax().text_range().into(),
                    )),
            );
        }
        (None, Some(headers_typ)) => {
            is_valid = false;

            ctx.reports.push(
                Report::error("missing `headers` field".to_string())
                    .add_label(Label::primary(
                        "missing `headers` field".to_string(),
                        obj.syntax().text_range().into(),
                    ))
                    .add_label(Label::secondary(
                        "headers type defined here".to_string(),
                        headers_typ.syntax().text_range().into(),
                    )),
            );
        }
        (Some(val), Some(typ)) => {
            let valid = value_matches_type(ctx, &val, &typ);
            if !valid {
                is_valid = false;
            }
        }
    }

    // Compare body
    let Some(body) = body_field.and_then(|f| f.value()) else {
        ctx.reports
            .push(
                Report::error("missin `body` field".to_string()).add_label(Label::primary(
                    "missin `body` field".to_string(),
                    obj.syntax().text_range().into(),
                )),
            );

        return false;
    };

    let Some(body_typ) = resp.body().and_then(|b| b.typ()) else {
        return is_valid;
    };

    let body_valid = value_matches_type(ctx, &body, &body_typ);
    if !body_valid {
        is_valid = false;
    }

    is_valid
}

/// Checks if content type is valid.
///
/// This is used when comparing object value with response type. At the time of writing this can only
/// happen when giving an example for a response type. It only makes sense to do so if the body is
/// a non-file type. In other words, content type of the response has to be `application/json`.
///
/// Caller of the main compare function is responsible for checking that comparison even makes sense.
/// This function assumes that request has contentType of application/json and enforces that.
fn is_content_type_valid(ctx: &mut Context, val: Option<ast::Value>) -> bool {
    let Some(val) = val else {
        return true;
    };

    let range = val.syntax().text_range();

    let ast::Value::String(string) = val else {
        ctx.reports.push(
            Report::error("response `contentType` field must be a string".to_string()).add_label(
                Label::primary(
                    "`contentType` must be a string".to_string(),
                    val.syntax().text_range().into(),
                ),
            ),
        );

        return false;
    };

    let token = string.string();
    let val_str = parse_string(&token, None);

    if val_str == "application/json" {
        return true;
    }

    ctx.reports.push(
        Report::error(format!(
            "`contentType` should be `application/json`, got `{val_str}`"
        ))
        .add_label(Label::primary(
            "expected `application/json`".to_string(),
            range.into(),
        )),
    );

    false
}

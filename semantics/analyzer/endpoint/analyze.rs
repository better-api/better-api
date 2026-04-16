use std::collections::HashMap;

use better_api_diagnostic::{Label, Report};
use better_api_syntax::TextRange;

use crate::analyzer::{LoweredSpec, RangeMap};
use crate::path::{Path, PathParamIterator, PathPart};
use crate::spec::Spec;
use crate::spec::view::endpoint::{EndpointView, RouteView};
use crate::spec::view::typ::{InlineTyView, TypeView};

impl LoweredSpec {
    pub(crate) fn validate_paths(&mut self) {
        validate_paths(&self.range_map, &mut self.reports, &self.spec);
    }
}

/// Validate endpoint and route paths.
///
/// Specifically this checks:
/// - endpoint paths are unique across the whole tree
/// - visible path params are unique in each scope
/// - endpoint path params type matches visible path params
fn validate_paths<'a>(range_map: &RangeMap, reports: &mut Vec<Report>, spec: &'a Spec) {
    // Unique paths and their ranges.
    let mut path_unique: UniquePathMap<'a> = HashMap::new();

    // Path params extract from path and descendants. Updated during traversal.
    let mut scoped_params: HashMap<&'a str, TextRange> = HashMap::new();

    // Shared buffer for type fields of Endpoint::path_params
    let mut fields_buf: HashMap<&'a str, TextRange> = HashMap::new();

    for endpoint in spec.root_endpoints() {
        validate_endpoint_path_unique(range_map, reports, &endpoint, &mut path_unique);

        let fields = PathParamTypeFields::from_endpoint_path(range_map, &mut fields_buf, &endpoint);

        validate_path_params(
            range_map,
            reports,
            &mut scoped_params,
            fields,
            &endpoint.path,
            |_, _, _| {},
        );
    }

    for route in spec.root_routes() {
        validate_route_paths_unique(range_map, reports, &route, &mut path_unique);
        validate_route_path_params(
            range_map,
            reports,
            &mut scoped_params,
            &mut fields_buf,
            &route,
        );
    }
}

/// Auxiliary method to validate if endpoint path is unique.
fn validate_endpoint_path_unique<'a>(
    range_map: &RangeMap,
    reports: &mut Vec<Report>,
    endpoint: &EndpointView<'a>,
    existing_paths: &mut UniquePathMap<'a>,
) {
    let path_id = endpoint.path.id();
    let path_range = range_map
        .path
        .get(&path_id)
        .expect("endpoint paths should be inserted into range map");

    if let Some(existing_range) = existing_paths
        .get(&endpoint.method)
        .and_then(|h| h.get(&endpoint.path))
    {
        reports.push(
            Report::error("duplicated endpoint path".to_string())
                .add_label(Label::primary(
                    "duplicated endpoint path".to_string(),
                    (*path_range).into(),
                ))
                .add_label(Label::secondary(
                    "same path first defined here".to_string(),
                    (*existing_range).into(),
                )),
        );
    } else {
        existing_paths
            .entry(endpoint.method.clone())
            .or_default()
            .insert(endpoint.path.clone(), *path_range);
    }
}

/// Auxiliary method to validate if route descendants have unique endpoint paths.
///
/// `existing_paths` is shared across all roots, so this enforces global uniqueness.
fn validate_route_paths_unique<'a>(
    range_map: &RangeMap,
    reports: &mut Vec<Report>,
    route: &RouteView<'a>,
    existing_paths: &mut UniquePathMap<'a>,
) {
    for endpoint in route.endpoints() {
        validate_endpoint_path_unique(range_map, reports, &endpoint, existing_paths);
    }

    for route in route.routes() {
        validate_route_paths_unique(range_map, reports, &route, existing_paths);
    }
}

/// Auxiliary method to validate route path params and all descendants.
///
/// This also maintains path params in scope while descending the tree.
fn validate_route_path_params<'a>(
    range_map: &RangeMap,
    reports: &mut Vec<Report>,
    scoped_params: &mut HashMap<&'a str, TextRange>,
    fields_buf: &mut HashMap<&'a str, TextRange>,
    route: &RouteView<'a>,
) {
    // Validate self and then children
    validate_path_params(
        range_map,
        reports,
        scoped_params,
        PathParamTypeFields::Ignore,
        &route.path,
        |range_map, reports, scoped_params| {
            // Validate child endpoints.
            for endpoint in route.endpoints() {
                let fields =
                    PathParamTypeFields::from_endpoint_path(range_map, fields_buf, &endpoint);

                validate_path_params(
                    range_map,
                    reports,
                    scoped_params,
                    fields,
                    &endpoint.path,
                    |_, _, _| {},
                );
            }

            // Validate child routes.
            for route in route.routes() {
                validate_route_path_params(range_map, reports, scoped_params, fields_buf, &route);
            }
        },
    );
}

/// Path parameter type fields that should be validated.
///
/// This wraps endpoint `path` attribute fields together with validation mode.
enum PathParamTypeFields<'a, 'b> {
    /// Don't validate path parameter type fields
    Ignore,

    /// There are no type fields
    None,

    /// There was an error during lowering of path parameter type
    Error,

    /// Path parameter type fields that should be validated
    Some {
        fields: &'b mut HashMap<&'a str, TextRange>,
        path_attribute_name_range: TextRange,
    },
}

impl<'a, 'b> PathParamTypeFields<'a, 'b> {
    fn from_endpoint_path(
        range_map: &RangeMap,
        fields_buf: &'b mut HashMap<&'a str, TextRange>,
        endpoint: &EndpointView<'a>,
    ) -> Self {
        fields_buf.clear();

        // If there is no params attribute name in endpoint, there for sure isn't any parameters.
        let Some(attr_name_range) = range_map.endpoint_path_attribute_name.get(&endpoint.id) else {
            return Self::None;
        };

        // We have parameter name, but we don't have a concrete lowered type.
        // This means there was an error during lowering of the path params type.
        let Some(mut path_param) = endpoint.path_param.clone() else {
            return Self::Error;
        };

        let record = loop {
            match path_param.typ() {
                TypeView::Inline(InlineTyView::NamedReference(reference)) => path_param = reference,
                TypeView::Record(record) => break record,

                _ => unreachable!("endpoint path parameter must be a reference to a simple record"),
            }
        };

        for field in record.fields() {
            let range = range_map
                .field_name
                .get(&field.id)
                .expect("record field name should be inserted into range map");
            fields_buf.insert(field.name.as_str(), *range);
        }

        Self::Some {
            fields: fields_buf,
            path_attribute_name_range: *attr_name_range,
        }
    }
}

type UniquePathMap<'a> = HashMap<http::Method, HashMap<Path<'a>, TextRange>>;

/// Validate that path parameters are unique and endpoint path param type is valid.
///
/// This function is used for endpoint and route path validation.
///
/// It adds params from current path segment to scope, validates descendants,
/// validates endpoint's [path type fields](Endpoint::path_param), then removes params introduced by this segment.
fn validate_path_params<'a, F>(
    range_map: &RangeMap,
    reports: &mut Vec<Report>,
    scoped_params: &mut HashMap<&'a str, TextRange>,
    path_param_fields: PathParamTypeFields<'a, '_>,
    path: &Path<'a>,
    mut validate_descendants: F,
) where
    F: FnMut(&RangeMap, &mut Vec<Report>, &mut HashMap<&'a str, TextRange>),
{
    let segment = match path.part() {
        PathPart::Empty => None,
        PathPart::Segment(seg) => Some(seg),
    };

    let range = range_map
        .path
        .get(&path.id())
        .expect("endpoint paths should be inserted into range map");

    // Populate map with params of the new segment
    if let Some(segment) = segment {
        let path_params = PathParamIterator::new(segment, *range);
        for (param, range) in path_params {
            match scoped_params.get(param) {
                None => {
                    scoped_params.insert(param, range);
                }
                Some(existing_range) => {
                    reports.push(
                        Report::error(format!("path parameter '{param}' already defined"))
                            .add_label(Label::primary(
                                "path parameter already defined".to_string(),
                                range.into(),
                            ))
                            .add_label(Label::secondary(
                                "previously defined here".to_string(),
                                (*existing_range).into(),
                            )),
                    );
                }
            }
        }
    }

    // Validate params of descendants, based on the inner function from caller.
    validate_descendants(range_map, reports, scoped_params);

    compare_path_params_to_type(reports, *range, scoped_params, path_param_fields);

    // Clean params from the hashmap
    if let Some(segment) = segment {
        let path_params = PathParamIterator::new(segment, *range);
        for (param, range) in path_params {
            let Some(stored_range) = scoped_params.get(param) else {
                continue;
            };

            // If ranges don't match, the param was defined by one of the parents,
            // which means we shouldn't be the ones removing it.
            if range == *stored_range {
                scoped_params.remove(param);
            }
        }
    }
}

/// Compare visible path parameters to endpoint's path param type.
///
/// It checks that all params in path are defined in the type,
/// and vice versa.
fn compare_path_params_to_type<'a>(
    reports: &mut Vec<Report>,
    path_range: TextRange,
    scoped_params: &HashMap<&'a str, TextRange>,
    path_param_fields: PathParamTypeFields<'a, '_>,
) {
    let (fields, path_attribute_range) = match path_param_fields {
        PathParamTypeFields::Ignore | PathParamTypeFields::Error => return,
        PathParamTypeFields::None => {
            if scoped_params.is_empty() {
                return;
            } else {
                reports.push(
                    Report::error("missing path parameters type".to_string()).add_label(
                        Label::primary(
                            "missing path parameters type".to_string(),
                            path_range.into(),
                        ),
                    ).with_note("help: endpoint's path contains path parameters. You must define their type in `path` attribute.".to_string()),
                );
                return;
            }
        }
        PathParamTypeFields::Some {
            fields,
            path_attribute_name_range,
        } => (fields, path_attribute_name_range),
    };

    for (p_name, p_range) in scoped_params.iter() {
        if fields.contains_key(p_name) {
            continue;
        }

        reports.push(
            Report::error(format!("missing field for path parameter `{p_name}`"))
                .add_label(Label::primary(
                    "missing field for path parameter".to_string(),
                    path_attribute_range.into(),
                ))
                .add_label(Label::secondary(
                    "path parameter defined here".to_string(),
                    (*p_range).into(),
                )),
        );
    }

    for (f_name, f_range) in fields.iter() {
        if scoped_params.contains_key(f_name) {
            continue;
        }

        reports.push(
            Report::error(format!("invalid path parameters field `{f_name}`"))
                .add_label(Label::primary(
                    "field not defined in path".to_string(),
                    (*f_range).into(),
                ))
                .with_note(
                    "help: endpoint's path attribute can only contain fields defined in the path"
                        .to_string(),
                ),
        );
    }
}

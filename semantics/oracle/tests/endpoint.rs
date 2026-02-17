use better_api_syntax::{ast, parse, tokenize};
use http::{Method, StatusCode};
use indoc::indoc;

use crate::{
    Oracle,
    path::PathPart,
    spec::{
        endpoint::{EndpointResponseType, ResponseStatus},
        typ::{InlineTy, PrimitiveTy, ResponseReference, SimpleRecordReference, Type},
    },
};

#[test]
fn lower_simple_endpoint() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert_eq!(oracle.reports(), vec![]);

    let mut root_endpoints = oracle.spec_ctx().root_endpoints();
    let endpoint = root_endpoints.next().expect("expected endpoint");
    assert!(root_endpoints.next().is_none());

    assert_eq!(endpoint.method, Method::GET);
    assert_eq!(endpoint.name, "foo");
    assert!(endpoint.path.segments().is_empty());
    assert!(endpoint.path_param.is_none());
    assert!(endpoint.query.is_none());
    assert!(endpoint.headers.is_none());
    assert!(endpoint.accept.is_none());
    assert!(endpoint.request_body.is_none());

    let responses: Vec<_> = endpoint.responses().collect();
    assert_eq!(responses.len(), 1);
    assert_eq!(responses[0].status, ResponseStatus::Code(StatusCode::OK));
    assert!(matches!(
        responses[0].typ,
        EndpointResponseType::InlineType(InlineTy::Primitive(PrimitiveTy::String))
    ));

    assert_eq!(oracle.spec_ctx().root_routes().count(), 0);
}

#[test]
fn lower_complex_valid_endpoint() {
    let text = indoc! {r#"
        type Query: rec {
            foo: string
        }

        POST {
            name: "foo"

            query: Query
            headers: {
                foo: string?
            }

            requestBody: rec {
                foo: string
                bar: [i32?]?
            }

            on default: resp {
                contentType: "image/png"
                body: file
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert_eq!(oracle.reports(), vec![]);

    let mut root_endpoints = oracle.spec_ctx().root_endpoints();
    let endpoint = root_endpoints.next().expect("expected endpoint");
    assert!(root_endpoints.next().is_none());

    assert_eq!(endpoint.method, Method::POST);
    assert_eq!(endpoint.name, "foo");
    assert!(endpoint.path.segments().is_empty());
    assert!(endpoint.path_param.is_none());
    assert!(endpoint.accept.is_none());

    let query = endpoint.query.clone().expect("expected query parameter");
    assert_eq!(query.name, "Query");

    let query_record = match query.typ() {
        SimpleRecordReference::SimpleRecord(record) => record,
        typ => panic!("expected simple record, got {typ:?}"),
    };
    let query_fields: Vec<_> = query_record.fields().collect();
    assert_eq!(query_fields.len(), 1);
    assert_eq!(query_fields[0].name, "foo");
    assert!(matches!(
        query_fields[0].typ,
        InlineTy::Primitive(PrimitiveTy::String)
    ));

    let headers = endpoint
        .headers
        .clone()
        .expect("expected headers parameter");
    assert_eq!(headers.name, "fooHeaders");

    let headers_record = match headers.typ() {
        SimpleRecordReference::SimpleRecord(record) => record,
        typ => panic!("expected simple record, got {typ:?}"),
    };
    let header_fields: Vec<_> = headers_record.fields().collect();
    assert_eq!(header_fields.len(), 1);
    assert_eq!(header_fields[0].name, "foo");

    let header_opt = match header_fields[0].typ.clone() {
        InlineTy::Option(opt) => opt,
        typ => panic!("expected option, got {typ:?}"),
    };
    assert!(matches!(
        header_opt.typ(),
        InlineTy::Primitive(PrimitiveTy::String)
    ));

    let request_body = endpoint
        .request_body
        .clone()
        .expect("expected request body");
    let request_body_ref = match request_body {
        InlineTy::NamedReference(reference) => reference,
        typ => panic!("expected named reference, got {typ:?}"),
    };
    assert_eq!(request_body_ref.name, "fooRequestBody");

    let request_body_record = match request_body_ref.typ() {
        Type::Record(record) => record,
        typ => panic!("expected record, got {typ:?}"),
    };
    let request_body_fields: Vec<_> = request_body_record.fields().collect();
    assert_eq!(request_body_fields.len(), 2);

    let foo_field = request_body_fields
        .iter()
        .find(|field| field.name == "foo")
        .expect("expected `foo` field");
    assert!(matches!(
        foo_field.typ,
        InlineTy::Primitive(PrimitiveTy::String)
    ));

    let bar_field = request_body_fields
        .iter()
        .find(|field| field.name == "bar")
        .expect("expected `bar` field");
    let bar_opt = match bar_field.typ.clone() {
        InlineTy::Option(opt) => opt,
        typ => panic!("expected option, got {typ:?}"),
    };
    let bar_arr = match bar_opt.typ() {
        InlineTy::Array(arr) => arr,
        typ => panic!("expected array, got {typ:?}"),
    };
    let bar_inner_opt = match bar_arr.typ() {
        InlineTy::Option(opt) => opt,
        typ => panic!("expected option, got {typ:?}"),
    };
    assert!(matches!(
        bar_inner_opt.typ(),
        InlineTy::Primitive(PrimitiveTy::I32)
    ));

    let responses: Vec<_> = endpoint.responses().collect();
    assert_eq!(responses.len(), 1);
    assert_eq!(responses[0].status, ResponseStatus::Default);

    let response_reference = match &responses[0].typ {
        EndpointResponseType::Response(reference) => reference,
        typ => panic!("expected response reference, got {typ:?}"),
    };
    assert_eq!(response_reference.name, "fooDefaultResponse");

    let response = match response_reference.typ() {
        ResponseReference::Response(response) => response,
        typ => panic!("expected response type, got {typ:?}"),
    };

    let content_types: Vec<_> = response
        .content_type
        .clone()
        .expect("expected content type")
        .collect();
    assert_eq!(content_types, vec!["image/png"]);
    assert!(matches!(
        response.body,
        InlineTy::Primitive(PrimitiveTy::File)
    ));
}

#[test]
fn lower_simple_valid_routes() {
    let text = indoc! {r#"
        route {
            on 404: string

            GET {
                name: "foo"

                on 200: string
            }
            route "/foo" {
                GET {
                    name: "bar"

                    on 200: string
                }
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert_eq!(oracle.reports(), vec![]);

    assert_eq!(oracle.spec_ctx().root_endpoints().count(), 0);

    let mut root_routes = oracle.spec_ctx().root_routes();
    let root_route = root_routes.next().expect("expected root route");
    assert!(root_routes.next().is_none());

    assert!(matches!(root_route.path.part(), PathPart::Empty));
    let root_responses: Vec<_> = root_route.responses().collect();
    assert_eq!(root_responses.len(), 1);
    assert_eq!(
        root_responses[0].status,
        ResponseStatus::Code(StatusCode::NOT_FOUND)
    );
    assert!(matches!(
        root_responses[0].typ,
        EndpointResponseType::InlineType(InlineTy::Primitive(PrimitiveTy::String))
    ));

    let root_endpoints: Vec<_> = root_route.endpoints().collect();
    assert_eq!(root_endpoints.len(), 1);
    assert_eq!(root_endpoints[0].method, Method::GET);
    assert_eq!(root_endpoints[0].name, "foo");
    assert!(root_endpoints[0].path.segments().is_empty());

    let foo_responses: Vec<_> = root_endpoints[0].responses().collect();
    assert_eq!(foo_responses.len(), 1);
    assert_eq!(
        foo_responses[0].status,
        ResponseStatus::Code(StatusCode::OK)
    );
    assert!(matches!(
        foo_responses[0].typ,
        EndpointResponseType::InlineType(InlineTy::Primitive(PrimitiveTy::String))
    ));

    let child_routes: Vec<_> = root_route.routes().collect();
    assert_eq!(child_routes.len(), 1);
    assert_eq!(child_routes[0].path.segments().as_slice(), &["/foo"]);
    assert_eq!(child_routes[0].responses().count(), 0);

    let child_endpoints: Vec<_> = child_routes[0].endpoints().collect();
    assert_eq!(child_endpoints.len(), 1);
    assert_eq!(child_endpoints[0].method, Method::GET);
    assert_eq!(child_endpoints[0].name, "bar");
    assert_eq!(child_endpoints[0].path.segments().as_slice(), &["/foo"]);

    let bar_responses: Vec<_> = child_endpoints[0].responses().collect();
    assert_eq!(bar_responses.len(), 1);
    assert_eq!(
        bar_responses[0].status,
        ResponseStatus::Code(StatusCode::OK)
    );
    assert!(matches!(
        bar_responses[0].typ,
        EndpointResponseType::InlineType(InlineTy::Primitive(PrimitiveTy::String))
    ));
}

#[test]
fn lower_invalid_endpoint_missing_name() {
    let text = indoc! {r#"
        GET {
            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_headers_param() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            headers: string

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_accept_type() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            accept: 32

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_missing_request_body() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            accept: ["image/png", "image/jpeg"]

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_request_body_not_file() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            accept: ["image/png", "image/jpeg"]
            requestBody: rec {
                foo: string
            }

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_request_body_contains_file() {
    let text = indoc! {r#"
        type Foo: rec {
            foo: file
        }

        GET {
            name: "foo"

            requestBody: rec {
                bar: Foo
            }

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_request_body_is_response() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            requestBody: resp {
                body: string
            }

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_paths_not_unique() {
    let text = indoc! {r#"
        GET "/foo/bar" {
            name: "foo"
            on 200: string
        }

        route "/foo" {
            route {
                GET "/bar" {
                    name: "bar"
                    on 200: i32?
                }
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_paths_params_not_unique() {
    let text = indoc! {r#"
        route "/foo" {
            route "/{slug}" {
                GET "/bar/{slug}" {
                    name: "foo"

                    path: {
                        slug: string
                    }

                    on 200: string
                }
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_paths_missing_path_attribute() {
    let text = indoc! {r#"
        GET "/foo/{bar}" {
            name: "foo"

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_paths_attribute_mismatch() {
    let text = indoc! {r#"
        route "/{foo}" {
            route "/{bar}" {
                GET "/baz/{xyz}" {
                    name: "foo"

                    path: {
                        foo: string
                        bar: i32
                        zyx: u32
                    }

                    on 200: string
                }
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_path_params_option_or_array() {
    let text = indoc! {r#"
        GET "/{foo}/{bar}" {
            name: "foo"
            path: {
                foo: i32?
                bar: [u64]
            }

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_invalid_endpoint_missing_response() {
    let text = indoc! {r#"
        GET {
            name: "foo"
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

#[test]
fn lower_warning_get_with_request_body() {
    let text = indoc! {r#"
        GET {
            name: "foo"

            requestBody: rec {
                foo: string
            }

            on 200: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    insta::assert_debug_snapshot!(oracle.reports());
}

fn setup_oracle<'a>(root: &'a ast::Root) -> Oracle<'a> {
    let mut oracle = Oracle::new_raw(root);
    oracle.validate_symbols();
    oracle.lower_type_definitions();
    oracle.lower_endpoints_and_routes();
    oracle.validate_paths();
    oracle
}

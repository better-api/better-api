# Better API

> [!NOTE]
> The project is still very much WIP.

Better API is (will be) a declarative language for specifying REST APIs, together with a toolkit for working with the
specifications. The toolkit includes generating server & client code from specifications, editor support for the language,
and a renderer for displaying the specification.

## Table of Contents

- [Motivation](#motivation)
- [Goals](#goals)
- [Syntax](#syntax)
  - [Preview](#preview)
- [Roadmap](#roadmap)

## Motivation

The industry standard for working with APIs is [OpenAPI](https://www.openapis.org/). It's great that we have a standard,
but I believe that there are some fundamental issues with it. I discussed those
[on my website](https://viddrobnic.com/writing/openapi-rant/). In the future I will probably distill these thoughts into
more concise and formal writing.

## Goals

The goals for this project are:

- **Spec First Design** - use declarative syntax that is easy for a human to read, write, and maintain.
- **First Party Tooling** - provide first-party tooling for parsing and validating the spec. The tooling
  must also be able to generate API server stubs and client code for multiple languages.
- **Great Developer Experience** - provide great developer experience for working with the spec.
  This includes syntax highlighting, LSP, and formatter.

The following are _not_ goals:

- **OpenAPI Compatibility** - Better API specs can be compiled to OpenAPI spec, but not all
  OpenAPI specs can be converted to Better API. This is by design.
- **XML** - Most (all?) modern APIs are JSON based. Better API's focus is on JSON APIs and won't
  support XML. [Go's xml library](https://pkg.go.dev/encoding/xml) gives a very good reason why:
  > Mapping between XML elements and data structures is inherently flawed: an XML element is an order-dependent
  > collection of anonymous values, while a data structure is an order-independent collection of named values.
  > See encoding/json for a textual representation more suitable to data structures.

## Syntax

The Better API syntax is described in the [syntax.md](docs/syntax.md).

> [!NOTE]
> The document is very crude and more in a "notes" format. It's not meant as official documentation or specification
> of the syntax.

### Preview

Here is a taste of what Better API looks like:

```text
/// This endpoint gets a greeting.
///
/// This comment starts with `///` and is a doc comment.
/// This means that when this spec is displayed with a renderer,
/// this comment shows up as documentation of the endpoint.
GET "/hello" {
  // Name of the endpoint, used in code generation.
  //
  // Notice that this comment starts with `//` and is a normal comment,
  // that is ignored by the toolkit.
  name: greeter

  // We will define some query params
  query: {
    /// Name of the person to greet.
    ///
    /// If no name is given, the whole world is greeted.
    name: string?
  }

  /// On success we return a greeting as a simple string.
  on 200: string

  // On error we return an object with details.
  // `rec` tells us this is a record. See syntax document for
  // more details.
  on 400: rec {
    problem: string
    details: string
  }
}
```

## Roadmap

- [x] parser
- [ ] semantic analysis
- [ ] code generation
- [ ] file uploads
- [ ] authentication & authorization
- [ ] expand number of supported languages

## License

The project is licensed under the [MIT License](LICENSE).

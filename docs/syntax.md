# Syntax

This document tries to semi-formally describe the syntax of the spec and some of the reasoning behind it.
It is very much a work in progress and will probably change during implementation of the tooling.

## Types

Because most programming languages don't support anonymous types, Better API doesn't support them either.
In practice this means that every response and request body has to be a named type (unless it's a [primitive type](#primitives)).
This way we can generate client code similar to:

```rust
struct Foo;

enum FooResponse {
  Status200(Foo),
  Fallback(Error),

}

fn foo(/* ... */) -> FooResponse {/* ... */}
```

### Primitives

The following primitive types are supported:

- **integers**: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- **floats**: `f32`, `f64`
- **date & time**:
  - `date`: ISO date string
  - `timestamp`: RFC3339 date-time string
- `bool`
- `string`
- `array`: denoted with `[<type>]`
- `file`: binary file with limitations on when and where it can be used.

### Composite Types

Composite types join [primitive](#primitives) and other composite types.

#### Structs

Structs are a way to define objects.

```text
type Foo struct {
  foo: string
  bar: i32
  baz: [timestamp]
}
```

##### Read & Write Only

Sometimes you want to use most of the struct as request and response body, but some fields
are different. For instance, when creating a resource with `POST` request you don't want to
specify and id, but you get it in response. For this, `@writeonly` and `@readonly` decorators can
be used

```text
type Foo struct {
  @readonly
  id: string

  @writeonly
  type: TypeEnum
}
```

#### Enums

Enums are a way to limit the options. They can limit integers or strings.

```text
type Foo enum(string) {
  foo
  bar
  "non-ident like value"
}

type Bar enum(i32) {
  -1
  42
}
```

#### Sum Types

Sum types, also known as tagged unions, are a way to create an object
that can be one of many objects based on a discriminator. Since everything is serialized into JSON,
types must be [structs](#structs). This is best described with an example:

```text
type FooStruct struct {
  foo_property: string
}

type BarStruct struct {
  bar_property: i32
}

type SumType union("type") {
  foo: FooStruct
  bar: BarStruct
}
```

This example can be serialized into:

```json
{
  "type": "foo",
  "foo_property": "string value"
}
```

or

```json
{
  "type": "bar",
  "bar_property": 10
}
```

You can change the name of the discriminator field by swapping the `"type"` in the
brackets with something else:

```text
type SumType union("my_discriminator") {
  // ...
}
```

### Aliases

Type aliases make it possible to change documentation and examples connected to a type.
We'll look at how to document types and add examples to them later.
Type alias documentation overrides the documentation of the original type.
Type alias examples are added to the original examples. Default example overrides the original
type's default example.

```text
type Alias: string
```

### Response & Request Body

Request and response bodies are the only types that can be anonymously declared as part of an [endpoint](#endpoint).
This is because they are not directly used to generate types in the target programming language.
You can still define them as a named type, for easier code reuse.

Usually, you will want to specify a request/response type when you want to reuse the same
documentation in multiple places, or when you want to override the default content type header in
multiple places. Setting an explicit content type is usually required only when working with files.
A more detailed description of when and why you want to change default content type is in
the [Working with Files](#working-with-files) section.

#### Response

Responses have a status code, content type header, additional headers, and response body type.
You can only specify the type of the response body. In this case, the status will be 200,
and the content type header will be `application/json`.

```text
type FooResponse response {
  type: Foo
}
```

is equal to

```text
type FooResponse response {
  status: 200
  contentType: "application/json"
  type: Foo
}
```

You can also specify an empty body:

```text
type FooResponse response {
  status: 204
  type: null
}
```

Adding custom headers is also possible:

```text
type FooResponse response {
  headers: {
    bar: string
    "X-Complex-Header": i32
  }

  type: Foo
}
```

#### Request Body

Request body declaration is very similar to response declaration, but without the status:

```text
type FooRequest requestBody {
  type: Foo
}
```

Or you can change the content type header:

```text
type FooRequest requestBody {
  contentType: "multipart/form-data"
  type: Foo
}
```

#### Fallback

You can specify a default error response. This is a response outside of the 2xx range that is not
covered explicitly. The fallback type is the same as response, but without the response code.
Usually it's not necessary to define a named fallback type.

```text
type FooFallback fallback {
  type: Error
}
```

### Examples

Examples can be defined only for a named type that is one of:

- [struct](#structs)
- [enum](#enums)
- [union](#sum-types)
- [alias](#aliases) to primitive type, struct, enum, or union.

Request/response body/fallback examples are not supported. If [struct](#structs) `Foo` is used as both
request body and response, the example of `Foo` should be a valid example for both request and response.
Otherwise, your type definitions are not consistent and you should define two types.

To define an example for request properties (path, header, query), you should define an [alias](#aliases) type
and define an example for the alias. This makes it consistent with [composite types](#composite-types) and also forces
code reuse instead of inlining everything.

Let's look at the most basic example:

```text
type Id: string

/// This is a simple example for ID
example for Id: "aa92e02f-1d4e-4228-b308-27451d0c45a1"
```

Examples can also be named:

```text
type Foo struct {
  bar: string
}

example "Foo" for Foo: {
  bar: "hello world!"
}
```

You can also specify an example as a default example. This is used mostly for display purposes.

```text
/// This is a default example
@default
example for Foo: "hey!"
```

## Path

Path is a segment that contains multiple [endpoints](#endpoint). You can look at it as a path prefix:

```text
path "/hello" {
  // ...
}
```

Paths can also be nested:

```text
path "/hello" {
  path "/world" {
    // ...
  }
}
```

## Endpoint

Endpoints are actual endpoints of the API. Usually it's most convenient to nest them inside a
[`path`](#path). This way you don't need to repeat the prefix.

Endpoints start with one or more methods, followed by an optional path. The following are equivalent:

```text
GET "/hello" {
  // ...
}

path "/hello" {
  GET {
    // ...
  }
}
```

The minimum required properties are:

- `name`: Name of the endpoint. This is used for code generation and has to be unique.
- `response`: At least one response type

```text
GET "/hello" {
  name: "hello"

  response: {
    type: Foo
  }

}
```

There can be more than one response, but each response has to have a unique `(status, contentType)` pair.
So you can define a 200 response and multiple 4xx and 5xx error responses. But you can't define
two 200 responses both returning an `application/json`.

You can specify almost any type as response (only requestBody and fallback are forbidden).
This means that instead of specifying an anonymous response type, you can specify ie. struct
directly, which gives a handy shorthand:

```text
GET {
  // ...
  response: Foo
}
```

You can also specify tags, path, query and header parameters, and request body. Here is
a more detailed example:

```text
POST "/hello/{id}" {
  name: "hello",

  // one tag
  tags: "one tag"
  // or multiple
  tags: ["one", "two"]

  path: {
    id: string
  }

  query: {
    name: string
  }

  headers: {
    "X-Header": string
    accept: string
  }

  // Default 200 response
  response: Greet

  // Error response
  response: {
    status: 404
    type: ErrorNotFound
  }

  fallback: ErrorFallback
}
```

Parameters in query, path, and headers are limited to [primitives](#primitives) (without `file`). This is because
there are many ways to serialize more complex types into URL path and query parameters.
Different languages use different libraries that do it in different ways. We want to keep compatibility
without annoying edge cases, so Better API supports only primitive types in these locations.

## Comments and Documentation

I already hinted at comments `// ...`. Better API also supports doc comments `/// ...`,
which is the primary way of adding documentation. Documentation can be added to:

- named types
- struct properties
- enum members
- discrete union members
- endpoints
- path, query, header parameters
- responses, request bodies and fallbacks

Let's look at an example:

```text
/// A greeting to the user
type Greeting struct {
  /// A greeting
  greeting: string
}

/// Greets the user
GET "/hello" {
  name: "greeter"

  path: {
    /// Name to greet
    name: string
  }

  /// Successfully greet the user
  response: Greeting
}
```

## Optionals and Defaults

Properties in [structs](#structs), path, query, and header parameters can be optional or have a default value.
If a property is optional, it can't also have a default.

Optional is declared by appending `?` to the end of the type.

```text
type Foo struct {
  bar: string?
}

GET "/foo" {
  // ...

  query: {
    bar: bool?
  }
}
```

Default is specified with a `@default` decorator:

```text
type Foo struct {
  @default("baz")
  bar: string
}

GET "/foo" {
  // ...

  query: {
    @default(42)
    bar: i32
  }
}
```

## Working with Files

TODO: Describe working with files and when changing the content type is desirable.

## Meta Data

TODO: Describe meta data like name, description, version, server urls

## Error Handling

TODO: Describe default fallback, overriding default fallback per path/endpoint scope

## Security

TODO: Describe security schemas

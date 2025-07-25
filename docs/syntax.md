# Syntax

This document tries to semi-formally describe the syntax of the spec and some of the reasoning behind it.
It is very much a work in progress and will probably change during implementation of the tooling.

## Basic Info

Let's start with some basic info that you can specify. This includes:

- version of Better API
- version of you API
- server URLs
- name of the API
- description of the API

```text
//! This is a special doc comment that is used to describe the whole API.
//! It can have multiple lines and everything.

// Version of better api spec
betterApi: "1.0"

// Name of your api
name: "My API"

// Version of you API. THis can be general string, which allows
// you to choose any versioning you like.
version: "4.2.42"

/// Description of the server
server: {
  name: "development"
  url: "http://localhost"
}

// By defining the servers block multiple times,
// you can define multiple servers.
server: {
  name: "staging"
  url: "https://staging.example.com"
}
```

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
are different. For instance, when creating a resource with a `POST` request you don't want to
specify an id, but you get it in the response. For this, `@writeonly` and `@readonly` decorators can
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
  foo,
  bar,
  "non-ident like value",
}

type Bar enum(i32) {
  -1,
  42,
}
```

#### Sum Types

Sum types, also known as tagged unions, are a way to create an object
that can be one of many objects based on a discriminator. Since everything is serialized into JSON,
the types must be [structs](#structs). This is best described with an example:

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
Type alias examples are added to the original examples. A default example overrides the original
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
A more detailed description of when and why you want to change the default content type is in
the [Working with Files](#working-with-files) section.

#### Response

Responses have a status code, content type header, additional headers, and a response body type.
You can specify only the type of the response body. In this case, the status will be 200,
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

Request body declaration is very similar to response declaration, but without the status code:

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
a request body and response, the example of `Foo` should be a valid example for both request and response.
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

There can be more than one response, but each response has to have a unique `status`.
So you can define a 200 response and multiple 4xx and 5xx error responses. But you can't define
two 200 responses. One response can have multiple `contentType`s. See [Working with Files](#working-with-files)
for more details on when you might want this and how to use it.

You can specify almost any type as a response (only requestBody and fallback are forbidden).
This means that instead of specifying an anonymous response type, you can specify e.g. a struct
directly, which gives a handy shorthand:

```text
GET {
  // Response with 200 status and application/json content type.
  response: Foo
}
```

You can also specify tags, path, query and header parameters, and request body. Here is
a more detailed example:

```text
POST "/hello/{id}" {
  name: "hello"

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

There can be only one requestBody per endpoint. But the request body can have multiple `contentType`s
(similar to responses). See [Working with Files](#working-with-files) for more details on how to use this.

## Comments and Documentation

I already hinted at comments `// ...`. Better API also supports doc comments `/// ...`,
which are the primary way of adding documentation. Documentation can be added to:

- named types
- struct properties
- enum members
- members of discrete unions
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
If a property is optional, it cannot also have a default.

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

You can also specify default for more complex types:

```text
type Foo struct {
  a: i32
  b: string
}

type Bar struct {
  @default({
    a: 10
    b: "hey!"
  })
  foo: Foo

  @default([1, 2, 3])
  bar: [i32]
}
```

## Working with Files

The only real reason for changing the content type of a request or response is when you are working with files.
Better API has a primitive `file` type which represents a binary file. Because of how HTTP works, binary
files are only supported for certain content types.

### Responses

To respond with a binary file, you just set the type of the response to `file`. The content type
of that response represents the mime type of the file.

```text
GET {
  response: {
    contentType: "image/png"
    type: file
  }
}
```

You can specify multiple content types:

```text
GET {
  response: {
    contentType: ["image/png", "image/jpeg"]
    type: file
  }
}
```

or a wildcard like

```text
GET {
  response: {
    contentType: "image/*"
    type: file
  }
}
```

or even

```text
GET {
  response: {
    contentType: "*/*"
    type: file
  }
}
```

### Request Body

Similar to responses, you can specify the type of the request body to be `file` and set the content type.

```text
POST {
  requestBody: {
    contentType: "image/*"
    type: file
  }
}
```

Unlike response types, you can also specify `multipart/form-data` as the contentType
and set the `type` of the body to a simple struct with one or more of the struct fields as `file`.

```text
type Foo struct {
  id: string
  image: file
}

POST {
  requestBody: {
    contentType: "multipart/form-data"
    type: Foo
  }
}
```

A simple struct is a struct where all properties are a primitive type or enum.
In other words, it must be only one layer deep. This is because the fields are
encoded as form data, and we want to keep consistency between languages
and libraries.

Since `application/x-www-form-urlencoded` is to some extent a simplified `multipart/form-data`,
you can set that as the requestBody content type as well. But in this case, you cannot upload files,
only simple structs.

### Wildcards

When multiple content types are specified, or content type contains a wildcard, the generated
code will loosen the type safety.

For instance if request body has a wildcard, generated server stubs will contain a content type
header string in the request parameter. It is the server implementation responsibility to check
which content type it is. Similarly, the wildcards in response types will have to be set by the
server implementation correctly.

For the generated client code it works symmetrically.

> [!NOTE]
> Wildcards add flexibility but should be used sparingly. Usually it's better to explicitly specify
> the accepted content types.

## Authorization

Currently there are three supported authorization types:

- **HTTP Bearer** - credentials are passed to `Authorization` header as `Authorization: Bearer <token>`.
- **HTTP Basic** - credentials are passed to `Authorization` header as `Authorization: Basic <creds>`
  where `creds` is base64 encoded string `username:password`
- **API Keys** - Credentials are passed to custom header or query parameter.

You start by defining an authorization type:

```text
type HttpBearer auth {
  type: "http"
  scheme: "bearer"
}

type HttpBasic auth {
  type: "http"
  scheme: "basic"
}

type ApiKey auth {
  type: "api_key"
  header: "X-API-KEY"
  query: "api_key"
}
```

> [!NOTE]
> As you can see, API key can be defined to be present in either header or query.
> You can also specify just header or just query, and leave the other field alone.

After you have an authorization type, you can use it to specify auth for a single endpoint:

```text
GET "/protected" {
  auth: ApiKey
  // ...
}
```

You can also specify multiple auth types per endpoint. They are joined by `or` (at least one
of them has to match)

```text
GET {
  auth: [ApiKey, HttpBasic]
  // ...
}
```

> [!NOTE]
> Because of how the code gen works, you can't specify "anonymous" auth. All auth types
> have to be named, similar to how all response types have to be named.

You can also specify that an auth is optional for and endpoint. This is useful when you have
a public endpoint, that shows additional info if user is logged in. This is done with the `?`
operator.

```text
// Optional auth method
GET {
  auth: ApiKey?
  // ...
}

// Multiple auth methods, but all are optional
GET {
  auth: [ApiKey, HttpBasic]?
  // ...
}
```

### Permissions

Endpoints can require that a user has permissions

```text
GET {
  auth: ApiKey
  permissions: "read"

  // ...
}
```

or multiple permissions:

```text
GET {
  auth: ApiKey
  permissions: ["read", "write"]

  // ...
}
```

> [!NOTE]
> Where the permissions come from is the concern of the server implementation. Bearer auth
> might read them from a JWT token, while API key auth might read them from the database.
> It is up to the server implementation to decide this.

If auth is optional, permissions can still be defined. If user is authorized, but doesn't have
required permissions, the generated code will treat them as unauthorized user.

### Scopes

Authorization and permissions can be declared for the whole path

```text
path "/protected" {
  auth: ApiKey
  permissions: "read"

  /// This endpoint is protected by the path's auth
  GET "/foo" {
    // ...
  }

  /// So is this one
  PUT "/bar" {
    // ...
  }
}
```

Inner path or endpoint can override the authorization and/or permissions.

```text
path "/general_protection" {
  auth: ApiKey
  permissions: "read"

  /// Requires ApiKey with permission "read"
  GET "/foo" {
    // ...
  }

  path "/strict" {
    permissions: "write"

    /// Requires ApiKey with permission "write"
    GET "/bar" {
      // ...
    }

    /// Requires ApiKey with permission "admin"
    GET "/baz" {
      permissions: "admin"
      // ...
    }
  }

  /// Requires ApiKey, but doesn't care about permissions
  GET "/" {
    auth: ApiKey
    permissions: null
    // ...
  }

  /// Doesn't require any permissions or auth
  GET "/unauthenticated" {
    auth: null
    permissions: null
    // ...
  }

  /// All endpoints inside this path are now unauthenticated.
  path "/ignore" {
    auth: null
    permissions: null
    // ...
  }
}
```

Auth and permissions are read for an endpoint. If one (or both) of them are not set,
they are read from the parent path. If that doesn't have them set, the parent is considered again.
And so forth until the top.

> [!NOTE]
> If you set auth to `null`, permissions should also be set to `null`.

### Default Auth

You can set default auth and permissions in a `defaults` block

```text
type ApiKey auth {
  // ...
}

defaults: {
  auth: ApiKey
  permissions: "read"
}
```

## Error Handling

We already mentioned that you can specify error responses for individual endpoints.
Similar to auth, `fallback` and `response` are scoped within endpoints, paths,
and the `defaults` block.

> [!TIP]
> It's recommended that you specify a `fallback` in the `defaults` block. This way
> the generated server and client code will know how to handle errors that might always
> happen (400, 500, ...).

Here is an example of how scoped errors might be defined:

```text
defaults: {
  fallback: Error
}

path "/protected" {
  auth: Bearer

  // We specify a default unauthorized error, since
  // all endpoints in this path are protected.
  response: {
    status: 401
    type: Error
  }

  // In this endpoint we don't have to specify fallback or
  // unauthorized error, but the generated code will have them.
  GET "/foo" {
    // ...
  }
}
```

Or, if your whole API is behind auth, you can do:

```text
defaults: {
  auth: Bearer

  response: {
    status: 401
    type: Error
  }

  fallback: Error
}
```

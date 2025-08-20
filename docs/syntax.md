# Syntax

This document tries to semi-formally describe the syntax of the spec and some of the reasoning behind it.
It is very much a work in progress and will probably change during implementation of the tooling.

## Table of Contents

- [Basic Info](#basic-info)
- [Types](#types)
  - [Primitives](#primitives)
  - [Composite Types](#composite-types)
  - [Aliases](#aliases)
  - [Response](#response)
  - [Examples](#examples)
- [Path](#path)
- [Endpoint](#endpoint)
- [Comments and Documentation](#comments-and-documentation)
- [Optionals and Defaults](#optionals-and-defaults)
- [Working with Files](#working-with-files)
  - [Responses](#responses)
  - [Request Body](#request-body-1)
  - [Wildcards](#wildcards)
- [Error Handling](#error-handling)
- [Authentication & Authorization](#authentication--authorization)
  - [Permissions](#permissions)
  - [Scopes](#scopes)
  - [Default Auth](#default-auth)
  - [Advanced Permissions](#advanced-permissions)
- [Code Generation Naming](#code-generation-naming)

## Basic Info

Let's start with some basic info that you can specify. This includes:

- version of Better API
- version of your API
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

// Version of your API. This can be a general string, which allows
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

### Primitives

The following primitive types are supported:

- **integers**: `i32`, `i64`, `u32`, `u64`
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

#### Records

Records are a way to define objects.

```text
type Foo: rec {
  foo: string
  bar: i32
  baz: [timestamp]
}
```

#### Enums

Enums are a way to limit the options. They can be integers or strings.

```text
type Foo: enum(string) {
  foo,
  bar,
  "non-ident like value",
}

type Bar: enum(i32) {
  -1,
  42,
}
```

#### Sum Types

Sum types, also known as tagged unions, are a way to create an object
that can be one of many objects based on a discriminator. Since everything is serialized into JSON,
the types must be [records](#records). This is best described with an example:

```text
type FooRecord: rec {
  foo_property: string
}

type BarRecord: rec {
  bar_property: i32
}

type SumType: union("type") {
  foo: FooRecord
  bar: BarRecord
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
type SumType: union("my_discriminator") {
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

### Response

You can use any JSON serializable type (everything that doesn't contain a `file`) as an endpoint response.
If you want to add additional headers or change the content type, you want to use the `response` type.
A more detailed description of when and why you want to change the default content type is in
the [Working with Files](#working-with-files) section.

If the response contains no additional headers and doesn't specify a custom content type, you can
and _should_ work with body types directly, instead of creating an unnecessary response type.

Response types can be assigned to a named type, as all other types can:

```text
type FooResponse: resp {
  body: Foo
}
```

is equal to using just `Foo` in the endpoint specifications. We'll look at endpoints later.

Some things you want to use a response type for are:

- specifying an empty body:
  ```text
  type FooResponse: resp {
    status: 204
    body: null
  }
  ```
- adding custom headers:
  ```text
  type FooResponse: resp {
    headers: FooHeaders
    body: Foo
  }
  ```

Sometimes it's handy to use anonymous (inlined) headers and body:

```text
type FooResponse: resp {
  headers: rec {
    bar: string
    "X-Complex-Header": i32
  }

  body: rec {
    foo: string
  }
}
```

When working with anonymous types, concrete types will still have to be generated in the server stubs
and client implementation. See [Code Generation Naming](#code-generation-naming) to see which names are reserved.
You can also just run the validator over your specifications and it will point you to names that clash
with generated names.

### Examples

Examples can be defined only for a named type that is one of:

- [records](#records)
- [enums](#enums)
- [unions](#sum-types)
- [aliases](#aliases) to primitive type, record, enum, or union.

Request/response body examples are not supported. If type `Foo` is used as both
a request body and response, the example of `Foo` should be a valid example for both request and response.
Otherwise, your type definitions are not consistent and you should define two types.

To define an example for request properties (path, header, query), you should define an [alias](#aliases) type
and define an example for the alias. This makes it consistent with [composite types](#composite-types) and also forces
code reuse instead of inlining everything.

Let's look at the most basic example:

```text
type Id: string

/// This is a simple example for Id
example for Id: "aa92e02f-1d4e-4228-b308-27451d0c45a1"
```

Examples can also be named:

```text
type Foo: rec {
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
- `on <status>`: At least one response type

```text
GET "/hello" {
  name: "hello"

  on 200: Foo
}
```

There can be more than one response, but each response has to have a unique status.
So you can define a 200 response and multiple 4xx and 5xx error responses. But you can't define
two 200 responses. One response can have multiple `contentType`s. See [Working with Files](#working-with-files)
for more details on when you might want this and how to use it.

You can also specify a default response with `on default: T`, which describes how a response looks like for
all other status codes that aren't explicitly defined. This is useful for handling other errors that are not
expected to happen, but you still want to handle them with type safety.

You can specify almost any type as a response. If you specify something that isn't a `resp`, it's treated
as `application/json` content type with no additional headers.

You can also specify tags, path, query and header parameters, and request body. Here is
a more detailed example:

```text
POST "/hello/{id}" {
  name: "hello"

  // one tag
  tags: "one tag"
  // or multiple
  tags: ["one", "two"]

  path: rec {
    id: string
  }

  query: rec {
    name: string
  }

  headers: rec {
    "X-Header": string
    accept: string
  }

  // Request body definition
  requestBody: HelloRequestBody

  // Default 200 response
  on 200: Greet
  on 404: ErrorNotFound
  on default: DefaultError
}
```

Parameters in query, path, and headers are limited to [primitives](#primitives) (without `file`). This is because
there are many ways to serialize more complex types into URL path and query parameters.
Different languages use different libraries that do it in different ways. We want to keep compatibility
without annoying edge cases, so Better API supports only primitive types in these locations.

There can be only one `requestBody` per endpoint. Endpoint can accept request body that is not
`application/json`. This is useful when working with files. See
[Working with Files](#working-with-files) for more details on how to do this.

This is also a good place to mention that there is some type inference when types are obvious. For instance,
path, query and header params can only be a record, therefore we can omit the `rec` keyword.

```text
POST "/hello/{id}" {
  name: "hello"
  tags: "one tag"

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
  on 200: Greet
  on 404: ErrorNotFound
}
```

## Comments and Documentation

We already hinted at comments `// ...`. Better API also supports doc comments `/// ...`,
which are the primary way of adding documentation. Documentation can be added to:

- named types
- record properties
- enum members
- members of sum types
- endpoints
- path, query, header parameters
- responses and request bodies

Let's look at an example:

```text
/// A greeting to the user
type Greeting: rec {
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
  on 200: Greeting
}
```

## Optionals and Defaults

Properties in [records](#records), path, query, and header parameters can be optional or
have a default value. If a property is optional, it cannot also have a default value.

Optional is declared by appending `?` to the end of the type.

```text
type Foo: rec {
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
type Foo: rec {
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
type Foo: rec {
  a: i32
  b: string
}

type Bar: rec {
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

The only reason for changing the content type of a request or response is when you are working with files.
Better API has a primitive `file` type which represents a binary file. Because of how HTTP works, binary
files are only supported for certain content types.

### Responses

To respond with a binary file, you just set the type of the response to `file`. The content type
of that response represents the mime type of the file.

```text
GET {
  on 200: resp {
    contentType: "image/png"
    body: file
  }
}
```

You can specify multiple content types:

```text
GET {
  on 200: resp {
    contentType: ["image/png", "image/jpeg"]
    body: file
  }
}
```

or a wildcard like

```text
GET {
  on 200: resp {
    contentType: "image/*"
    body: file
  }
}
```

or even

```text
GET {
  on 200: resp {
    contentType: "*/*"
    body: file
  }
}
```

### Request Body

Similar to responses, you can specify the type of the request body to be `file` and set the content type
that the endpoint accepts. To set the content type, use `accept` field.

```text
POST {
  accept: "image/*"
  requestBody: file
}
```

Unlike response types, you can also specify `multipart/form-data` as the `accept`
and set the `type` of the body to a simple record with one or more of the record fields as `file`.

```text
type Foo: rec {
  id: string
  image: file
}

POST {
  accept: "multipart/form-data"
  requestBody: Foo
}
```

A simple record is a record where all properties are a primitive type or enum.
In other words, it must be only one layer deep. This is because the fields are
encoded as form data, and we want to keep consistency between languages
and libraries.

Since `application/x-www-form-urlencoded` is to some extent a simplified `multipart/form-data`,
you can set that as the `accept` value as well. But in this case, you cannot upload files,
only simple records.

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

## Error Handling

We already mentioned that you can specify error responses for individual endpoints.
To make working with errors easier, Better API makes responses scoped
within endpoints, paths, and the `defaults` block.

> [!TIP]
> It's recommended that you specify a default response in the `defaults` block. This way
> the generated server and client code will know how to handle errors that might always
> happen (400, 500, ...).

Here is an example of how scoped errors might be used for defining an API that
has some endpoints rate limited:

```text
defaults: {
  on default: Error
}

path "/limited" {
  // We specify a default rate limiting error, since
  // all endpoints in this path are rate limited.
  on 429: ErrorTooManyRequests

  // In this endpoint we don't have to specify fallback or
  // rate limited error, but the generated code will have them.
  GET "/foo" {
    // ...
  }

  // This endpoint has a special fallback error and overrides
  // the default fallback error.
  GET "/bar" {
    on default: SpecialError
  }
}
```

Or, if your whole API is rate limited, you can do:

```text
defaults: {
  on 429: ErrorTooManyRequests
  on default: Error
}
```

## Authentication & Authorization

Currently there are three supported authentication types:

- **HTTP Bearer** - credentials are passed to `Authorization` header as `Authorization: Bearer <token>`.
- **HTTP Basic** - credentials are passed to `Authorization` header as `Authorization: Basic <creds>`
  where `creds` is base64 encoded string `username:password`
- **API Keys** - credentials are passed to custom header or query parameter.

Each authentication method must also define an Unauthorized error type. This is the body of a
401 response returned if the authentication against the method fails.

Authentication method can also define a Forbidden error type. This is the body of a
403 response returned if a permission check against the auth method fails. This is
required if an endpoint specifies required permissions. Permissions are discussed later.

You start by defining an authentication type:

```text
type HttpBearer: auth {
  type: "http"
  scheme: "bearer"
  unauthorized: UnauthorizedError
}

type HttpBasic: auth {
  type: "http"
  scheme: "basic"
  unauthorized: UnauthorizedError
}

type ApiKey: auth {
  type: "api_key"
  header: "X-API-KEY"
  query: "api_key"

  unauthorized: UnauthorizedError
  forbidden: ForbiddenError
}
```

> [!NOTE]
> As you can see, API key can be defined to be present in either header or query.
> You can also specify just header or just query, and leave the other field empty.

After you have an authentication type, you can use it to specify auth for a single endpoint:

```text
GET "/protected" {
  auth: ApiKey
  // ...
}
```

Endpoint being authenticated means that 401 response for it is already defined. Defining
a 401 response for such an endpoint results in an error.

---

You can also specify multiple auth types per endpoint. They are joined by `or` (at least one
of them has to match).

```text
GET {
  auth: [ApiKey, HttpBasic]
  // ...
}
```

For endpoints that have multiple auth methods, all methods have to have the same unauthorized error
types.

> [!NOTE]
> Because of how the code gen works, you can't specify anonymous auth. All auth types
> have to be named.

---

You can also specify that an auth is optional for an endpoint. This is useful when you have
a public endpoint that shows additional info if user is logged in. This is done with the `?`
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

If auth is optional, permissions can still be defined. If a user is authenticated but doesn't have
required permissions, the generated code will treat them as an unauthorized user.

If an endpoint has multiple auth methods and requires permissions, at least _one_ auth method has to authorize
the user with _all_ required permissions. Otherwise the user is treated as unauthorized or unauthenticated.
In this case all auth methods have to have the same forbidden error type.
This is similar to unauthorized error types.

Defining permissions on an endpoint also defines its 403 response, similar to how defining an auth method
defines a 401 response. This means that an endpoint can't redefine a 403 response.

### Scopes

Authentication and permissions can be declared for the whole path

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

Inner path or endpoint can override the authentication and/or permissions.

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
they are read from the parent path. If that doesn't have them set, the parent is considered again,
and so forth until the top.

> [!NOTE]
> If you set auth to `null`, permissions must also be set to `null`.

> [!NOTE]
> Specifying auth and permissions for a path defines 401 and 403 responses for all endpoints in the path.

### Default Auth

You can set default auth and permissions in a `defaults` block

```text
type ApiKey: auth {
  // ...
}

defaults: {
  auth: ApiKey
  permissions: "read"
}
```

### Advanced Permissions

There are cases when simple "read" permission is not enough. For instance, you have an endpoint
`/pets/{id}` that returns a pet by id and only an owner can update the pet. In this case you can
define only an authentication method, without any permissions, and a custom 403 response.

```text
PUT "/pets/{id}" {
  auth: Bearer

  on 403: ForbiddenError

  // ...
}
```

This way you can perform additional permission checks in the endpoint business logic, where
you have access to the pet for the specified id and authenticated user.

## Code Generation Naming

TODO

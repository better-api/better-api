# Motivation

The industry standard for working with APIs is [OpenAPI](https://www.openapis.org/). Most people who have had to define
or consume an OpenAPI spec have a subpar experience that can, and should, be improved.
Most of the pain of working with OpenAPI comes from the combination of:

- OpenAPI wants to support everything - JSON, [XML](https://swagger.io/docs/specification/v3_0/data-models/representing-xml/),
  form data, enums ([oneOf](https://swagger.io/docs/specification/v3_0/data-models/oneof-anyof-allof-not/#oneof)),
  unions ([allOf](https://swagger.io/docs/specification/v3_0/data-models/oneof-anyof-allof-not/#allof)),
  something in between ([anyOf](https://swagger.io/docs/specification/v3_0/data-models/oneof-anyof-allof-not/#anyof)),
  [nulls](https://swagger.io/docs/specification/v3_0/data-models/data-types/#null), undefined, and the list goes on and on.
- OpenAPI does not really concern itself with tooling. Yes, they have a list of third-party tools. But at the end of
  the day it's just a spec of how a giant JSON/YAML file should be structured. How that file is created, edited,
  viewed, and consumed is none of its concern.

The combination of these two facts makes OpenAPI files difficult to produce and consume _correctly_. It's fine for
writing a spec, but when you actually want to _implement_ it in a programming language it starts to fall apart.

There are many examples of this, but to keep the motivation behind the project short let's focus on one of the most
obvious and often encountered examples. When defining an OpenAPI object, you can define some properties as required
and some as optional. Additionally, some properties can be [`nullable`](https://swagger.io/docs/specification/v3_0/data-models/data-types/#null).
In practice that means that OpenAPI has a distinction between `null` and `undefined`. In some instances this is actually
useful for defining a spec. But when you want to implement such a spec in a language that isn't JS/TS this is undesired.

Most languages (Go, Rust, Java, Python, ...) don't actually differentiate between the two - they just have some form
of `nil`/`None`/`null`. To handle the OpenAPI `null`/`undefined` separation, you need to wrap all the properties in additional
wrapper types with custom JSON (de)serialization logic. Obviously, such a custom wrapper type is not the most pleasant
thing to work with. Most tooling takes the easy way out by using a "language default" JSON library which parses both
`undefined` and `null` as whatever version of `null` the language has (`nil` in Go, `None` in Rust, ...).

Another issue is language inconsistencies. Because of a very general specification and no official tooling,
there are often inconsistencies in tooling for different languages. For instance, server stubs generated for one
language can be incompatible with generated client code in another language.

There are many more examples of OpenAPI supporting types that are difficult to work with correctly, most of them being a
side effect of [polymorphism](https://swagger.io/docs/specification/v3_0/data-models/inheritance-and-polymorphism/).
So you constantly reach for simplifications, which are usually fine. Until they aren't. And then you have a bad day and start
asking yourself how OpenAPI has been an industry standard for more than a decade and the tooling around it still sucks.

There are some efforts to make the tooling better, most notably [typespec](https://typespec.io/) and [smithy](https://smithy.io/).
But I believe that such efforts don't solve the fundamental problem of OpenAPI being too detached from programming
languages in which APIs and their clients are actually implemented. So I decided to try to make a better alternative
that prioritizes tooling and keeps in mind the fact that an API has to be implemented in some programming language.

# Goals

- **Spec First Design** - use declarative syntax that is easy for a human to read, write, and maintain.
- **First Party Tooling** - provide first-party tooling for parsing and validating the spec. The tooling
  must also be able to generate API server stubs and client code for multiple languages.
- **Great Developer Experience** - provide great developer experience for working with the spec.
  This includes syntax highlighting, LSP, and formatter.

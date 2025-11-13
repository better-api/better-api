## TODO

- [ ] Implement analysis in the `Oracle`:
  - [ ] Construct a symbol table.
  - [ ] Implement symbol table resolution (name -> Type). Do not forget to detect cycles, ie:
    ```text
    type Foo: Bar
    type Bar: Foo
    ```
  - [x] Put all types in a type arena & put values from prologues to type arena.
  - [x] Put symbol mapping to source map.
  - [ ] Check default value matches the type.
  - [ ] Check type is correct based on context where it's used (union, enum, ...). For this I might
        need to define `route` and `endpoint` semantic elements first.

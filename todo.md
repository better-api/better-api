## TODO

- [ ] Define symbol table data structure. Symbol table maps `semantics::Element` to `SyntaxNodePtr` and
      in reverse `TextRange` to `semantics::Element`.
- [ ] Implement analysis in the `Oracle`:
  - [ ] Construct a symbol table.
  - [ ] Put all types in a type arena & put values from prologues to type arena.
  - [ ] Put symbol mapping to source map.
  - [ ] Check default value matches the type.
  - [ ] Check type is correct based on context where it's used (union, enum, ...). For this I might
        need to define `route` and `endpoint` semantic elements first.

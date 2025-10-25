## TODO

- [x] handle array/option type/value being dropped inside object/record/union...
  - remove the whole field
  - make the array zero sized (what about option?)
- [ ] test value and type arena by:
  - build dummy arenas
  - try builder works and representation in arena is correct
  - try .get() and iterators work as expected (especially nested types)
  - try Drop on builders work (especially inside of nested types)

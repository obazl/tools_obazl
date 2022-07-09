# modules field of library stanza

This field implicitly refers to the source files in the directory, so
we cannot test it independently. A library with no `modules` field
listed includes all modules in the directory, so we need a project
with some structfiles and sigfiles.

* `(modules foo bar)` just the modules listed, which must be in the dir
* `(modules (:standard))` - include all modules in the directory.
* no `modules` field: same as `(modules (:standard))`.  this is common.
* `(modules (:standard \ foo bar))` - include all modules except those listed
* empty `modules` (i.e. `(modules)`) - exclude all.


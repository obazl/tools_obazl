
/*
https://dune.readthedocs.io/en/stable/concepts.html#preprocessing-spec

"Dune accepts three kinds of preprocessing:

* no_preprocessing, meaning that files are given as it to the compiler, this is the default
* (action <action>) to preprocess files using the given action
* (pps <ppx-rewriters-and-flags>) to preprocess files using the given list of ppx rewriters
* (staged_pps <ppx-rewriters-and-flags>) is similar to (pps ...) but behave slightly differently and is needed for certain ppx rewriters (see below for details)
* future_syntax is a special value that brings some of the newer OCaml syntaxes to older compilers. See Future syntax for more details
"

Er, I count five, but never mind. Missing from this list is 'per_module'.

(preprocess (per_module
             (((action (run ./pp.sh X=1 %{input-file})) foo bar))
             (((action (run ./pp.sh X=2 %{input-file})) baz))))

A (preprocess...) sexp maps to a struct stanza_s as follows:

name := "preprocess"

val.pp := struct pp_s


*/

struct pp_s {
    int type;                   /* NOP, ACTION, PPS, STAGED_PPS, FUTURE_SYNTAX */
    union {
        struct action_s *action;
        UT_array *pps;          /* string array of flags and ppx lib names */
    }
}

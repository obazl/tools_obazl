
/*
  Currently we do not parse 'action' sexps, we just pass them through as text.
 */

/*
https://dune.readthedocs.io/en/stable/concepts.html#id7

(action ...) fields describe user actions.

User actions are always run from the same subdirectory of the current build context as the dune file they are defined in. So for instance an action defined in src/foo/dune will be run from $build/<context>/src/foo.

The argument of (action ...) fields is a small DSL that is interpreted by dune directly and doesn’t require an external shell.

...

The following constructions are available:

* (run <prog> <args>) to execute a program. <prog> is resolved locally if it is available in the current workspace, otherwise it is resolved using the PATH

* (dynamic-run <prog> <args>) to execute a program that was linked against dune-action-plugin library. <prog> is resolved in the same way as in run

* (chdir <dir> <DSL>) to change the current directory

* (setenv <var> <value> <DSL>) to set an environment variable

* (with-<outputs>-to <file> <DSL>) to redirect the output to a file, where <outputs> is one of: stdout, stderr or outputs (for both stdout and stderr)

* (ignore-<outputs> <DSL>) to ignore the output, where <outputs> is one of: stdout, stderr or outputs

* (with-stdin-from <file> <DSL>) to redirect the input from a file

* (with-accepted-exit-codes <pred> <DSL>) specifies the list of expected exit codes for the programs executed in <DSL>. <pred> is a predicate on integer values, and is specified using the Predicate language. <DSL> can only contain nested occurrences of run, bash, system, chdir, setenv, ignore-<outputs>, with-stdin-from and with-<outputs>-to. This action is available since dune 2.0.

* (progn <DSL>...) to execute several commands in sequence

* (echo <string>) to output a string on stdout

* (write-file <file> <string>) writes <string> to <file>

* (cat <file>) to print the contents of a file to stdout

* (copy <src> <dst>) to copy a file

* (copy# <src> <dst>) to copy a file and add a line directive at the beginning

* (system <cmd>) to execute a command using the system shell: sh on Unix and cmd on Windows

* (bash <cmd>) to execute a command using /bin/bash. This is obviously not very portable

* (diff <file1> <file2>) is similar to (run diff <file1> <file2>) but is better and allows promotion. See Diffing and promotion for more details

* (diff? <file1> <file2>) is similar to (diff <file1> <file2>) except that <file2> should be produced by a part of the same action rather than be a dependency, is optional and will be consumed by diff?.

* (cmp <file1> <file2>) is similar to (run cmp <file1> <file2>) but allows promotion. See Diffing and promotion for more details

* (no-infer <DSL>) to perform an action without inference of dependencies and targets. This is useful if you are generating dependencies in a way that Dune doesn’t know about, for instance by calling an external build system.

* (pipe-<outputs> <DSL> <DSL> <DSL>...) to execute several actions (at least two) in sequence, filtering the <outputs> of the first command through the other command, piping the standard output of each one into the input of the next. This action is available since dune 2.7.

*/

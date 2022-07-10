#!/bin/sh

## For local development. Symlinks the scheme files for mibl and libs7
## to XDG_HOME_DATA so that they can be edited within their repos.
## Copies the executable (repl -> mibl) and libc_s7.so instead of
## symlinking, since they are located after build in a Bazel-controled
## directory. Assumes that `https://github.com/obazl/libs7` is cloned
## to the local system.

## Usage: $ bazel build convert && .tools/symlink.sh

mkdir -vp $HOME/.local/share/mibl
mkdir -vp $HOME/.local/share/mibl/dune
mkdir -vp $HOME/.local/share/mibl/meta
mkdir -vp $HOME/.local/share/mibl/opam
mkdir -vp $HOME/.local/share/mibl/s7
mkdir -vp $HOME/.local/share/mibl/starlark

echo "Linking scripts"
ln -sfv $HOME/obazl/libs7/libs7/*.scm $HOME/.local/share/mibl
ln -sfv $HOME/obazl/libs7/libs7/s7/*.scm $HOME/.local/share/mibl/s7

ln -sfv $HOME/obazl/mibl/mibl/*scm $HOME/.local/share/mibl
ln -sfv $HOME/obazl/mibl/mibl/dune/*.scm $HOME/.local/share/mibl/dune
ln -sfv $HOME/obazl/mibl/mibl/meta/*.scm $HOME/.local/share/mibl/meta
ln -sfv $HOME/obazl/mibl/mibl/opam/*.scm $HOME/.local/share/mibl/opam

ln -sfv $HOME/obazl/tools_obazl/obazl/starlark.scm $HOME/.local/share/mibl
ln -sfv $HOME/obazl/tools_obazl/obazl/starlark/*scm $HOME/.local/share/mibl/starlark

## copy the executable and lib (do not symlink from Bazel directories)
cp -fv `realpath bazel-bin/repl/repl` $HOME/.local/bin/obazl
chmod u+rwx $HOME/.local/bin/obazl

# cp -fv `realpath bazel-bin/external/libs7/src/libc_s7.so` $HOME/.local/share/mibl
# chmod u+rwx $HOME/.local/share/mibl/libc_s7.so

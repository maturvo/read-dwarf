# <Insert name here>

The goal of this project is to be able to symbolically execute binaries
using the `isla` project and then prove (as) automatically (as possible)
that two different optimisation levels have the exact same behavior.
This is done by using intensively DWARF debugging information, in particular
C type information. The project will only work for binaries generated from C.

## Dependencies

### Build dependencies
There is one dependency that is not on opam: `isla-lang`. Go to [the `isla-lang`
repository](https://github.com/rems-project/isla-lang) and install the latest
version. `opam install .` should work there.

Then you can either install the default configuration of read-dwarf with opam,
with `opam install .` or just install other dependencies with
`opam install --deps-only .` and then follow the rest of the README


### Run time dependencies

You need a more or less recent version `z3`, if don't have it on your system
package manager, you can use `opam install z3`.

You also need to install `isla`.  Warning for
non-Rust users: you need to add `~/.cargo/bin` to your PATH or specify the
position of `isla-client` in `ISLA_CLIENT_PATH` for `read-dwarf` to find it.

## Configuration

TODO: this has changed a bit, but configuration shouldn't be needed for simple use.

There is no configure script. To edit the compile time configuration,
copy `default_config.ml` into `config.ml` and edit the variables you wish
to change from the default. If `config.ml` is not found, the build system
will use `default_config.ml` instead.

In particular you can change the supported architecture for the build by editing
the `module Arch` variable to point to the module you want in `src/archs`.

The run time configuration is in `config.toml`, but for now during development
it is checked out in the repository.

## Building

A simple `make` works. A `read-dwarf` symlink will then be created.

While developing, `make merlin` will only build the necessary parts for the
merlin plugin to work. It will fail later than plain `make` allowing a more
accurate linting.

## Installing

To install `read-dwarf`, you can do `dune install` without opam or
just use opam and do `opam install .`.

`dune uninstall` also work.

## Auto-formatting

To have auto-formatting you need exactly version 0.12 of `ocamlformat`.
For example one could do:

`opam pin ocamlformat 0.12` and `opam install ocamlformat`.

Then you can use `make format` to format your code.
Please always format before committing.
Ask everyone if you want to change ocamlformat options or bump the version.


## Documentation

`make doc` builds the automatic documentation that is then accessible from
`doc.html`.

The `odoc` program (`opam install odoc`) is required.

Extra documents are available in the `doc` folder.

## Usage

You can run `./read-dwarf --help` to get the list of subcommand. For each
subcommand you can run `./read-dwarf subcommand --help` to learn more about
the subcommand

## Testing

You can run `make test` for self testing.
The libraries `ounit`, `qtest` and `qcheck` are additionally required.
You can get then from opam:

```
opam install qtest ounit qcheck
```

There are three types of tests that are all called as part of the `make test`
command:
 - Inline tests: They appear as qtest comments in the source code like `(*$=
   test *)`. They can be built alone with `make rd-inline-tester` and called
   alone with `make inline-test`
 - Library tests: Those are tests testing read-dwarf as a library. They live in
   the `tests` folder. They are built as a single executable with `make
   rd-tester` and can be run directly with `make lib-test`. However by
   running `rd-tester`, one has more options listed with `./rd-tester --help`. 
   This is especially useful to debug when they fail.
 - Command tests: Those are tests testing read-dwarf as a command line tool.
   They are just specific command listed in `make test` body. You can just run
   commands individually if required.
   
The first two sets of test are registered with dune and will be called by
commands like `dune test` and `make dune-test`. A little caveat is that the
inline test and the library tests are both randomized with different seed. The
seeds are printed on stdout, but they should not be confused with one another.
 
## Folder structure

 - `src`: The OCaml sources
   - `dune`: The dune configuration file
   - `.merlin`: Auto generated merlin file, do not commit.
   - `intro.smt2`: Common SMT definition used when calling a SMT solver.
     This is embedded in the binary.
   - `archs`: All the architecture specific code. Chose one in `config.ml`.
   - `ast.ott`: The ott syntax of the internal language
 - `default_config.ml`: Default configuration
 - `config.ml`: Current configuration (may be omitted to use `default_config.ml`)
 - `config.toml`: Runtime configuration
 - `tests.`: Library test suite. It compiles as a single executable that performs the tests
 - `dune-project`: Global dune configuration
 - `dune`: Some other dune configuration for the `config.ml` file
 - `read-dwarf.opam`: OPAM file. Generated by `dune-project`, edit `dune-project`.
 - `notes`: Various notes and thought about the project development.
 - `doc`: Documentation related element
 - `TODO.md`: The current TODO list.
 - `hafnium*/*`: Each folder is a specific hafnium build to test on.
   The binaries are frozen and should not be changed.
 - `compare-O0-O2`: Folder for graphs comparing `O0` and `O2` hafnium builds.
 - `test_asm`: Simple assembly test to test `run-bb` subcommand.
 - `tiny_test`: Simple C tests, see the corresponding README inside the folder.
 - `mpool`: Folder to build some mpool specific tests
 - `emacs-highlighting`: Emacs coloring for `read-dwarf rd` output.
   Add this snippet at the beginning of such outputs and follow the instructions.

## Caching

Read-dwarf may create a cache in a directory named `.rdcache`. When searching
for a cache, read-dwarf will search if there already is a `.rdcache` directory
either in the current directory or one of its parent and use the closest one it
finds. If it finds none and need a cache, it will create a .rdcache in the
current directory. This directory contains several caches indexed by name.

Use `read-dwarf cache --clear name` to delete such a cache or
`read-dwarf cache --clear --a` to clear the whole `.rdcache` directory.
You also do `read-dwarf cache --list` to list existing caches.

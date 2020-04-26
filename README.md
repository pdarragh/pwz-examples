# PwZ Examples

A straightforward implementation of Parsing with Zippers.

This library was built using OCaml version 4.05.0. It has no other external
dependencies.

## Code Organization

The code is organized into a few key files.

`types.ml` defines the types used by various files. Specifically, this is where
the type aliases for labels, tags, positions, and tokens are defined, as well as
the recursive datatype definitions of expressions (`exp`) and contexts (`cxt`).
This file also defines the "bottom" instantiations of the various types, which
are used as dummy values during expression construction, among other things.

`pwz.ml` is the implementation of the algorithm described in the paper.

`cst.ml` defines an additional type (`cst`) and functions for extracting
concrete syntax trees from completed parses resulting from calls to `Pwz.parse`.
It is used for convenience, and so is not considered part of the algorithm
proper.

`grammars.ml` provides a small test suite for PwZ. In it are defined a number of
tricky grammars that provide evidence for our claim that PwZ is a general
parsing algorithm capable of parsing any context-free grammar. These grammar
definitions include tests that can be run by invoking the module's
`print_test_results` function.

`repl.ml` simply pulls the modules into an interactive OCaml REPL session. We
intend for this to make it easy to play with the test grammars we have provided
(or any others you may choose to implement). This file can be invoked directly
from the command line. With a default OCaml installation, you can do
`ocaml -init repl.ml` to be placed in an interactive session with this code
available. However, we recommend using the
[UTop toplevel](https://opam.ocaml.org/blog/about-utop/) as it is much more
fully-featured. If you have UTop installed, you can invoke the REPL script by
doing `utop -init repl.ml`. The script includes some more specifics about how to
use the functionality it provides, which will be printed to your terminal when
first starting your interactive session.

`.merlin` simply enables Merlin support within the directory. To use Merlin
requires you to compile the source files, so we also provide a `Makefile` to
automate the compilation. Simply running `make` should successfully compile all
of the source files. (There will not be a specific output file, as there is no
executable associated with this code.)

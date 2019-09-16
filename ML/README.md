CS 4110 Homework 3
==================

See the associated `instructions.pdf` for details on the assignment.


The Files
---------

- `ast.ml`:
  Defines the data types for the abstract syntax trees (ASTs).

- `eval.ml`:
  The interpreter for the ASTs. Your task is to edit this (and
  only this) file to A lexer and parser for IMP programs.

- `main.ml`:
  The top level code that parses in an input file, and executes it.

- `lexer.mll` and `parser.mly`:
  The lexer and parser specs for IMP programs. These are fed through [OCaml's
  lexer and parser generators][ocamlyacc] to generate source code.

- `pprint.ml`:
  A pretty printer for the ASTs.

- `test.imp`:
  A test IMP program.

[ocamlyacc]: http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html


Compile and Execute
-------------------

You'll need to [install OCaml][] if you don't have it already. For example, use `brew install ocaml` on macOS, the [official OCaml installer][ocaml-win] on Windows, and your package manager on most Linuxes.

If you have Make (i.e., on any Unix), you can simply type `make` at the
command line. Compilation will produce an executable file
called `imp`.

Now you can run the executable on a test program: `./imp test.imp`.

This will parse the file test.imp and evaluate the program.

[install OCaml]: https://ocaml.org/docs/install.html
[ocaml-win]: http://protz.github.io/ocaml-installer/


Working with OCaml
------------------

OCaml modes exist for most editors:

* Vim: comes built in with recent versions, or
  http://www.ocaml.info/vim/ftplugin/ocaml.vim
* Emacs: http://caml.inria.fr/pub/docs/u3-ocaml/emacs/index.html
* Atom: https://atom.io/packages/language-ocaml
* VSCode: https://marketplace.visualstudio.com/items?itemName=hackwaly.ocaml
* Sublime Text: has a built in mode, or a better one at
  https://github.com/whitequark/sublime-better-ocaml

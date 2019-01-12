# Extract test code from OCaml doc-comments.

ocamlcodoc extracts the preformatted source code in OCaml
documentation comments, i.e. the code delimited by `{[ ... ]}` in
comments delimited by `(** ... *)`. A typical usage is to write examples
in documentation comments that can be extracted and tested.

ocamlcodoc can be used as an [inline test](https://dune.readthedocs.io/en/latest/tests.html#inline-tests) back-end with dune:
```
(inline_tests (backend ocamlcodoc))
```

## Basic usage

By default, ocamlcodoc acts as a filter from standard input to standard output:

```
$ cat source.ml
let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
(**
   The Fibonacci function.
   {[
     assert (fib 0 = 1);
     assert (fib 1 = 1);
     assert (fib 10 = 89);
   ]}
 *)
$ ocamlcodoc <source.ml >extracted.ml
$ cat extracted.ml 

# 7 "stdin"

     assert (fib 0 = 1);
     assert (fib 1 = 1);
     assert (fib 10 = 89);
```

Source files can be given as command-line arguments:

```
$ ocamlcodoc source.ml

# 7 "source.ml"

     assert (fib 0 = 1);
     assert (fib 1 = 1);
     assert (fib 10 = 89)
```

All preformatted source code snippets are concatenated.

```
$ cat source.ml
let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
(**
   The Fibonacci function.

   Initial cases:
   {[
     assert (fib 0 = 1);
     assert (fib 1 = 1);
   ]}

   Some other test cases:
   {[
     assert (fib 10 = 89);
     assert (fib 20 = 10946);
   ]}
 *)
$ ocamlcodoc source.ml

# 9 "source.ml"

     assert (fib 0 = 1);
     assert (fib 1 = 1);
   
# 15 "source.ml"

     assert (fib 10 = 89);
     assert (fib 20 = 10946);
```

## Insert code not appearing in the documentation

Code outside documentation comment can be inserted with `(*{[ ... ]}*)`.

```
$ cat source.ml
(*{[ open Fib ]}*)

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
(**
   The Fibonacci function.

   Initial cases:
   {[
     assert (fib 0 = 1);
     assert (fib 1 = 1);
   ]}

   Some other test cases:
   {[
     assert (fib 10 = 89);
     assert (fib 20 = 10946);
   ]}
 *)
$ ocamlcodoc source.ml

# 1 "source.ml"
 open Fib 
# 11 "source.ml"

     assert (fib 0 = 1);
     assert (fib 1 = 1);
   
# 17 "source.ml"

     assert (fib 10 = 89);
     assert (fib 20 = 10946);
```

## Set output file

Output file name can be given as command-line argument with the option `-o`.
`-o -` targets standard output (the default).

```
$ ocamlcodoc source.ml -o extracted.ml
```

The output file name can be a pattern where `%` stands for the base
name (without suffix) of each input file name. If the pattern path is
relative, the reference path is the path of the input file.

If two input files or more are given, then all extracted code will be
concatenated, except if the output file name is a pattern: in this
case, there will be one output file per input file.

## Warnings and errors

Comments, preformatted source code and strings should be terminated
before the end of the input file.

A common mistake is to end prematurely a preformatted source code with
`]}`, by closing a list or an array inside a record. This should be
written by separating delimiters with a space `] }`.  Otherwise,
the resulting extracted code will be probably ill-formed. `ocamlcodoc`
will issue a warning in this case, except if the delimiters become
balanced in subsequent snippets. For example, the following code
produces a warning:
```
(*** {[ { list = ["a"; ]} *)
```
whereas the following code does not:
```
(*** {[ { list = ["a"; ]} (some comment) {[ "b" ] } ]} *)
```
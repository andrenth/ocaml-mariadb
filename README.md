# OCaml-MariaDB

## Introduction

OCaml-MariaDB is a library containing
[Ctypes](https://github.com/ocamllabs/ocaml-ctypes)-based bindings for MariaDB.
The library provides access to the traditional MySQL blocking API via the
`Mariadb.Blocking` module, as well as the MariaDB nonblocking API, via
`Mariadb.Nonblocking`, which is designed mainly for use with OCaml's monadic
concurrent programming libraries such as [Lwt](https://ocsigen.org/lwt) and
[Async](https://github.com/janestreet/async).

Only the prepared-statement APIs are exposed by OCaml-MariaDB, as these
functions provide typed query parameters and database field access.

## Installation

OCaml-MariaDB requires MariaDB's client library version 5.5.21 or greater or
the C connector library version 2.1.0 or greater (but version 3.0.0 or greater
is recommended). If your distribution has these already packaged those versions,
simply install either package. For example, on Debian or Ubuntu, run

```console
# apt-get install libmariadbclient-dev
```

to use the client library, or

```console
# apt-get install libmariadb-dev
```

to use the C connector.

In case both are installed, OCaml-MariaDB will link against C connector.

If your distribution doesn't yet package those versions, you can either install
them manually, set up a third-party package archive (for example, there's an
[Ubuntu PPA](https://launchpad.net/~jonathonf/+archive/ubuntu/mysql) that
provides version 2.3.1 of the C connector), or configure MariaDB's own
[repositories](https://downloads.mariadb.org/mariadb/repositories/), from which
the client library will be available.

To install OCaml-MariaDB via [OPAM](https://opam.ocaml.org/) simply type

```console
$ opam install mariadb
```

To install it manually, type

```console
$ ./configure
$ ocaml setup.ml -build
$ ocaml setup.ml -install

If you want to build the Lwt and/or Async example programs, pass respectively
`--enable-lwt` and `--enable-async` to the `configure` command above.
OCaml-Mariadb itself has no dependency on either of those libraries.
```

## The blocking API

OCaml-MariaDB's API should be familiar to those who have used other MySQL
libraries in OCaml or other languages before. A query must be initially
*prepared*, resulting in a prepared statement, which can then be *executed*
when given an appropriate set of parameters. Statement execution leads to a
query *result* which can then be used to *fetch* rows, one at a time.

A simple example is given below.

```ocaml
open Printf

module M = Mariadb.Blocking

let or_die = function
  | Ok x -> x
  | Error (num, msg) -> failwith (sprintf "error #%d: %s" num msg)

let main () =
  let mariadb =
    M.connect
      ~host:"localhost"
      ~user:"myuser"
      ~pass:"secret"
      ()
    |> or_die in
  let query = "SELECT * FROM mysql.users WHERE Host LIKE ? LIMIT ?" in
  let stmt = M.prepare mariadb query |> or_die in
  let res = M.Stmt.execute stmt [| `String "%"; `Int 10 |] |> or_die in
  printf "number of rows: %d\n%!" (M.Res.num_rows res);
  print_rows res; (* see below *)
  M.Stmt.close stmt |> or_die;
  M.close mariadb;
  (* Call this only once, before you're done using all
     your database handles. *)
  M.library_end ()

let () = main ()
```

## The nonblocking API

Usage of the nonblocking API is very similar to the blocking one, but designed
to support OCaml's popular monadic concurrency libraries (though usage of a
monadic library is not mandatory -- see the `examples` directory for an example
using `Unix.select`).

To use the nonblocking API, a module of type `Mariadb.Nonblocking.Wait` must be
provided to the functor `Mariadb.Nonblocking.Make`. This module must contain
an asynchronous I/O type definition along with the usual *bind* and *return*
monadic operations, as well as a `wait` function that specifies how to wait for
the MariaDB socket to become readable and/or writable.

The signature is as follows.

```ocaml
module type Wait = sig
  module IO : sig
    type 'a future

    val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
    val return : 'a -> 'a future
  end

  val wait : t -> Mariadb.Nonblocking.Status.t
          -> Mariadb.Nonblocking.Status.t IO.future
end
```

The `wait` function receives a *status* parameter that specifies which socket
events are to be waited for (which can be checked via `Status.read`,
`Status.write` and `Status.timeout` -- see the `ocamldoc` for more details).
It must then return a new status, specifying which of those events have actually
occurred.

A simple example of the nonblocking library usage is given below. Full examples
of wait modules for Lwt and Async can be found in the `examples` directory.

```ocaml
module M = Mariadb.Nonblocking.Make(struct
  module IO = struct
    type 'a future = ...
    let (>>=) m f = ...
    let return x = ...
  end

  let wait mariadb status =
    ...
end)

let main () =
  M.connect
    ~host:"localhost"
    ~user:"myuser"
    ~pass:"secret"
    ()
  >>= or_die
  >>= fun mariadb ->
  let query = "SELECT * FROM mysql.users WHERE Host LIKE ? LIMIT ?" in
  M.prepare mariadb query >>= or_die
  >>= fun stmt ->
  M.Stmt.execute stmt [| `String "%"; `Int 10 |] >>= or_die
  >>= fun res ->
  print_rows res >>= fun () -> (* see below *)
  M.Stmt.close stmt >>= or_die >>= fun () ->
  M.close mariadb >>= fun () ->
  M.library_end ()
```

## Fetching rows

Rows can be fetched using the `Res.fetch` function. This function takes a
module as its first parameter that defines the data structure in which the
row is to be fetched.

For example,

```ocaml
M.Res.fetch (module M.Row.Array) res
```

returns the row as an `array` of `Field.t` values. The following built-in
modules are provided with OCaml-MariaDB, but the user is free to implement
one if so desired, in which case it must conform to the `Row.S` module type
(see the `ocamldoc` for details).

* `Row.Array`: fetch row as `Field.t array`;
* `Row.Map`: fetch row as a map of column name (`string`) to `Field.t`;
* `Row.Hashtbl`: fetch row as a `(string, Field.t) Hashtbl.t`.

The `fetch` function returns a `row option result`, where `row` represents the
row type given by the module argument, and `result` is a wrapper for the
`Pervasives.result` type that carries an `error` in the `Error` case. In the
`Ok` case, `fetch` returns `Some row`, containing the next available row, or
`None`, in which case no more rows are available in the result.

## Reading fields

A database field is represented by the `Field.t` type, and its value by the
`Field.value` type, which can be obtained by calling the `Field.value`
function.

```ocaml
type value =
  [ `Int of int
  | `Float of float
  | `String of string
  | `Bytes of bytes
  | `Time of Time.t
  ]
```

The `Field.value` function can also return `` `Null`` in case the field is an
SQL `NULL`.

Since the type of a field is in most cases known beforehand, as the user must
be aware of the table definition in order to query it, helper functions are
provided to extract the OCaml types directly from a field:

```ocaml
val int : Field.t -> int
val float : Field.t -> float
val string : Field.t -> string
val bytes : Field.t -> bytes
val time : Field.t -> Time.t
```

These functions will raise an exception if the field value is not of the
expected type, but as noted above this shouldn't be a problem.

For nullable fields, the following analogous functions are also provided:

```ocaml
val int_opt : Field.t -> int option
val float_opt : Field.t -> float option
val string_opt : Field.t -> string option
val bytes_opt : Field.t -> bytes option
val time_opt : Field.t -> Time.t option
```

These functions return `None` if the field value is `` `Null``, or `Some v`
otherwise.

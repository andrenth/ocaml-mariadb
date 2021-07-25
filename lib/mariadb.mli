(** OCaml-MariaDB is a library with OCaml bindings for MariaDB's
    libmysqlclient, including support for its nonblocking API. While you
    need MariaDB's libmysqlclient to build OCaml-MariaDB, it should be
    possible to use it with the regular libmysqlclient from MySQL,
    as long as you don't try to use the nonblocking API.

    These bindings are restricted to MariaDB's prepared statement APIs,
    as they provide support for typed query parameters and results.
*)

(** The MariaDB blocking interface. *)
module type S = sig
  type error = int * string
    (** The type of errors that can result from MariaDB API calls. *)

  type 'a result = ('a, error) Stdlib.result
    (** The result of MariaDB API calls. *)

  (** Module representing MariaDB date- and time-related values. *)
  module Time : sig
    type t
      (** The type of time values. *)

    (** {2 Retrieval of time components} *)

    val year : t -> int
    val month : t -> int
    val day : t -> int
    val hour : t -> int
    val minute : t -> int
    val second : t -> int
    val microsecond : t -> int

    (** {2 Creation of time values} *)

    val time : hour:int -> minute:int -> second:int
            -> ?microsecond:int -> unit -> t
    val local_timestamp : float -> t
    val utc_timestamp : float -> t
    val date : year:int -> month:int -> day:int -> unit -> t
    val datetime : year:int -> month:int -> day:int
                -> hour:int -> minute:int -> second:int
                -> ?microsecond:int -> unit -> t
  end

  (** This module defines a database field retrieved by a query. *)
  module Field : sig
    type t
      (** The type of fields. *)

    type value =
      [ `Null
      | `Int of int
      | `Float of float
      | `String of string
      | `Bytes of bytes
      | `Time of Time.t
      ]

    val name : t -> string
      (** [name field] returns the field name of [field]. *)

    val value : t -> value
      (** [value field] returns the value associated with [field]. *)

    val null_value : t -> bool
      (** [null_value field] returns [true] if the value associated with
          [field] is [NULL]. *)

    val can_be_null : t -> bool
      (** [can_be_null field] returns [true] if values of [field] can assume
          the [NULL] value (i.e. the table definition does not specify
          [NOT NULL] for this field. *)

    (** {2 Value retrieval functions}

        The functions below simplify the unwrapping of OCaml values from
        fields. They raise [Failure] if the field is not of the expected
        type, but this should not be a problem as the type of a field is
        in all likelyhood known in advance by database users.
    *)

    val int : t -> int
    val float : t -> float
    val string : t -> string
    val bytes : t -> bytes
    val time : t -> Time.t

    val int_opt : t -> int option
    val float_opt : t -> float option
    val string_opt : t -> string option
    val bytes_opt : t -> bytes option
    val time_opt : t -> Time.t option
  end

  (** A module representing database rows. Rows can be retrieved as different
      data structures, which as passed to the row retrieval functions from the
      [Res] module. There's built-in support for fetching rows as arrays and
      maps and hash tables of field name to field, but any module conforming
      to [Row.S] can be provided to those functions. *)
  module Row : sig
    module type S = sig
      type t
        (** The type of database rows. *)

      val build : int -> (int -> Field.t) -> t
        (** [build n f] creates a row of [n] fields built by the results of
            [f 0], [f 1], ..., [f (n-1)]. *)
    end

    module StringMap : Map.S with type key = string

    module Array : (S with type t = Field.t array)
      (** Rows as field arrays. *)

    module Map : (S with type t = Field.t StringMap.t)
      (** Rows as field name to [Field.t] maps. *)

    module Hashtbl : (S with type t = (string, Field.t) Hashtbl.t)
      (** Rows as field name to [Field.t] hash tables. *)
  end

  (** The module containing operations on MariaDB query results. *)
  module Res : sig
    type t
      (** The type of query results. *)

    val num_rows : t -> int
      (** [num_rows res] returns the number of rows in result [res] after
          the execution of a [SELECT]-type query. *)

    val affected_rows : t -> int
      (** [num_rows res] returns the number of affected rows in result [res]
          after the execution of an [INSERT] or [UPDATE]-type query. *)

    val fetch : (module Row.S with type t = 'r) -> t -> 'r option result
      (** [fetch (module M : Row.S) res] fetches the next available row
          from [res], returning it in as the data structure specified by
          module [M]. Returns [None] when no more rows are available. *)
  end

  (** The module contains operations on MariaDB prepared statements. *)
  module Stmt : sig
    type t
      (** The type of prepared statement. *)

    val execute : t -> Field.value array -> Res.t result
      (** [execute stmt params] executes the prepared statement [stmt]
          binding to it the query parameters [params] and returns a [Res.t],
          the query result. *)

    val reset : t -> unit result
      (** [reset stmt] reset the client and server state of [stmt] to what they
          were after [stmt] was prepared, and frees up any {!Res.t} produced by
          [stmt]. *)

    val sqlstate : t -> string
      (** [sqlstate stmt] is the SQLSTATE with MariaDB extensions indicating the
          status of the previous execution of the statement. The string
          ["00000"] is returned if no error occurred or if the statement has not
          been executed. *)

    val close : t -> unit result
			(** [close stmt] closes the prepapred statement [stmt] and frees
					any allocated memory associated with it and its result. *)
  end

  type t
    (** The type of database handles. *)

  type flag =
    | Compress
    | Found_rows
    | Ignore_sigpipe
    | Ignore_space
    | Interactive
    | Local_files
    | Multi_results
    | Multi_statements
    | No_schema
    | Odbc
    | Ssl
    | Remember_options

  type protocol =
    | Default
    | Tcp
    | Socket
    | Pipe
    | Memory

  type client_option =
    | Connect_timeout of int
    | Compress
    | Named_pipe of string
    | Init_command of string
    | Read_default_file of string
    | Read_default_group of string
    | Set_charset_dir of string
    | Set_charset_name of string
    | Local_infile of bool
    | Protocol of protocol
    | Shared_memory_base_name of string
    | Read_timeout of int
    | Write_timeout of int
    | Secure_auth of bool
    | Report_data_truncation of bool
    | Reconnect of bool
    | Ssl_verify_server_cert of bool
    | Plugin_dir of string
    | Default_auth of string
    | Bind of string
    | Ssl_key of string
    | Ssl_cert of string
    | Ssl_ca of string
    | Ssl_capath of string
    | Ssl_cipher of string
    | Ssl_crl of string
    | Ssl_crlpath of string
    | Connect_attr_reset
    | Connect_attr_add of string * string
    | Connect_attr_delete of string
    | Server_public_key of string
    | Enable_cleartext_plugin of bool

  type server_option =
    | Multi_statements of bool

  val connect : ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> ?flags:flag list
             -> ?options:client_option list -> unit
             -> t result
		(** Connect to a MariaDB server at the specified location with the specified
				flags and optionally select a database [db]. *)

  val close : t -> unit
    (** Close a database handle. *)

  val library_end : unit -> unit
		(** [library_end ()] should be called when you're done using the
        library. For maximum portability across MariaDB C client libraries,
        call this function only once, after you've [close]d all database
        handles. *)

  val set_character_set : t -> string -> unit result
    (** Sets the connection character set to the given parameter. *)

  val select_db : t -> string -> unit result
    (** [select_db mariadb db] changes the current database to [db]. *)

  val change_user : t -> string -> string -> string option -> unit result
    (** [change_user mariadb user pass db] changes the connection user to
        [user] with password [password] and optionally change to database
        [db]. *)

  val set_client_option : t -> client_option -> unit
    (** Sets the given client option on the connection. *)

  val set_server_option : t -> server_option -> unit result
    (** Sets the given server option on the connection. *)

  val ping : t -> unit result
    (** Checks if the connection to the MariaDB server is working. If the
        [Reconnect] option is set and the connection is down, a reconnect
        attempt will be made. *)

  val autocommit : t -> bool -> unit result
    (** Sets autocommit mode on or off. *)

  val commit : t -> unit result
    (** Commits the current transaction. *)

  val rollback : t -> unit result
    (** Rolls back the current transaction. Does not work if autocommit is
        enabled. *)

  val prepare : t -> string -> Stmt.t result
		(** [prepare mariadb query] creates a prepared statement for [query].
				The query may contain [?] as placeholders for parameters that
				can be bound by calling [Stmt.execute]. *)

  val sqlstate : t -> string
    (* [sqlstate mariadb] is the SQLSTATE with MariaDB extensions of the last
     * operation on [mariadb]. Returns ["00000"] if no error occurred. *)
end

(** The module for blocking MariaDB API calls. It should be possible to call
    functions from this module when using MySQL's or an older version of
    MariaDB's libmysqlclient. *)
module Blocking : S

(** This is the nonblocking MariaDB API. The interface contains a functor
    [Make] which, given a way to wait for a connection socket to be ready
    for reading or writing, returns a module with the same signature [S]
    as the traditional blocking API. *)
module Nonblocking : sig
  module Status : sig
    type t
      (** The type of a nonblocking operation status. *)

    val create : ?read:bool
              -> ?write:bool
              -> ?except:bool
              -> ?timeout:bool
              -> unit -> t
      (** Create a new status indicating which events have occured on the
          MariaDB connection socket. *)

    val read : t -> bool
      (** Indicates if a read event has occurred. *)

    val write : t -> bool
      (** Indicates if a write event has occurred. *)

    val except : t -> bool
      (** Indicates if an exceptional condition event has occurred. *)

    val timeout : t -> bool
      (** Indicates if a timeout has occurred. *)
  end

  type t
    (** The type of nonblocking database handles. *)

  val fd : t -> Unix.file_descr
    (** The underlying file descriptor of the database connection. *)

  val timeout : t -> int
    (** If a nonblocking operation returns a [Status.t] indicating a timeout
        event, this function can be used to obtain the value, in seconds,
        after which the timeout has occured. *)

  val timeout_ms : t -> int
    (** Same as [timeout] but with millisecond resolution. *)

	(** Input module signature for the functor that generates a nonblocking
			connection module. *)
  module type Wait = sig
    (** A module defining a nonblocking I/O monadic interface. *)
    module IO : sig
      type 'a future

      val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
      val return : 'a -> 'a future
    end

    val wait : t -> Status.t -> Status.t IO.future
			(** [wait mariadb status] must wait for the events set in [status]
					to occur in the [mariadb] connection and return a [Status.t]
					indicating which events have actually occured. *)
  end

  (* The MariaDB nonblocking interface. The exact same functions in the
     blocking interface are available here, but operations that could
     otherwise block there return a [future] value here. *)
  module type S = sig
    type error = int * string
    type 'a future
    type 'a result = ('a, error) Stdlib.result

    module Time : sig
      type t

      val year : t -> int
      val month : t -> int
      val day : t -> int
      val hour : t -> int
      val minute : t -> int
      val second : t -> int
      val microsecond : t -> int

      val time : hour:int -> minute:int -> second:int
              -> ?microsecond:int -> unit -> t
      val local_timestamp : float -> t
      val utc_timestamp : float -> t
      val date : year:int -> month:int -> day:int -> unit -> t
      val datetime : year:int -> month:int -> day:int
                  -> hour:int -> minute:int -> second:int
                  -> ?microsecond:int -> unit -> t
    end

    module Field : sig
      type t

      type value =
        [ `Null
        | `Int of int
        | `Float of float
        | `String of string
        | `Bytes of bytes
        | `Time of Time.t
        ]

      val name : t -> string
      val value : t -> value
      val null_value : t -> bool
      val can_be_null : t -> bool

      val int : t -> int
      val float : t -> float
      val string : t -> string
      val bytes : t -> bytes
      val time : t -> Time.t

      val int_opt : t -> int option
      val float_opt : t -> float option
      val string_opt : t -> string option
      val bytes_opt : t -> bytes option
      val time_opt : t -> Time.t option
    end

    module Row : sig
      module type S = sig
        type t
        val build : int -> (int -> Field.t) -> t
      end

      module StringMap : Map.S with type key = string

      module Array : (S with type t = Field.t array)
      module Map : (S with type t = Field.t StringMap.t)
      module Hashtbl : (S with type t = (string, Field.t) Hashtbl.t)
    end

    module Res : sig
      type t

      val num_rows : t -> int
      val affected_rows : t -> int
      val fetch : (module Row.S with type t = 'r) -> t
               -> 'r option result future
    end

    module Stmt : sig
      type t

      val execute : t -> Field.value array -> Res.t result future
      val reset : t -> unit result future
      val close : t -> unit result future
    end

    type t

    type flag =
      | Compress
      | Found_rows
      | Ignore_sigpipe
      | Ignore_space
      | Interactive
      | Local_files
      | Multi_results
      | Multi_statements
      | No_schema
      | Odbc
      | Ssl
      | Remember_options

    type protocol =
      | Default
      | Tcp
      | Socket
      | Pipe
      | Memory

    type client_option =
      | Connect_timeout of int
      | Compress
      | Named_pipe of string
      | Init_command of string
      | Read_default_file of string
      | Read_default_group of string
      | Set_charset_dir of string
      | Set_charset_name of string
      | Local_infile of bool
      | Protocol of protocol
      | Shared_memory_base_name of string
      | Read_timeout of int
      | Write_timeout of int
      | Secure_auth of bool
      | Report_data_truncation of bool
      | Reconnect of bool
      | Ssl_verify_server_cert of bool
      | Plugin_dir of string
      | Default_auth of string
      | Bind of string
      | Ssl_key of string
      | Ssl_cert of string
      | Ssl_ca of string
      | Ssl_capath of string
      | Ssl_cipher of string
      | Ssl_crl of string
      | Ssl_crlpath of string
      | Connect_attr_reset
      | Connect_attr_add of string * string
      | Connect_attr_delete of string
      | Server_public_key of string
      | Enable_cleartext_plugin of bool

    type server_option =
      | Multi_statements of bool

    val connect : ?host:string
               -> ?user:string
               -> ?pass:string
               -> ?db:string -> ?port:int -> ?socket:string
               -> ?flags:flag list
               -> ?options:client_option list -> unit
               -> t result future

    val close : t -> unit future
    val library_end : unit -> unit
    val set_character_set : t -> string -> unit result future
    val select_db : t -> string -> unit result future
    val change_user : t -> string -> string -> string option
                   -> unit result future
    val set_client_option : t -> client_option -> unit
    val set_server_option : t -> server_option -> unit result future
    val ping : t -> unit result future
    val autocommit : t -> bool -> unit result future
    val commit : t -> unit result future
    val rollback : t -> unit result future
    val prepare : t -> string -> Stmt.t result future
  end

	(** Functor that generates a nonblocking database interface, given a
      nonblocking IO monad and a way to wait for connection socket events. *)
  module Make (W : Wait) : S with type 'a future := 'a W.IO.future
end

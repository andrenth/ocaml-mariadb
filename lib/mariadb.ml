module type S = sig
  type error = int * string
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
    val fetch : (module Row.S with type t = 'r) -> t -> 'r option result
  end

  module Stmt : sig
    type t

    val execute : t -> Field.value array -> Res.t result
    val reset : t -> unit result
    val sqlstate : t -> string
    val close : t -> unit result
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
             -> t result

  val close : t -> unit
  val library_end : unit -> unit
  val set_character_set : t -> string -> unit result
  val select_db : t -> string -> unit result
  val change_user : t -> string -> string -> string option -> unit result
  val set_client_option : t -> client_option -> unit
  val set_server_option : t -> server_option -> unit result
  val ping : t -> unit result
  val autocommit : t -> bool -> unit result
  val commit : t -> unit result
  val rollback : t -> unit result
  val prepare : t -> string -> Stmt.t result
  val sqlstate : t -> string
end

module B = Binding_wrappers

module Common = Common
module Blocking = Blocking
module Nonblocking = Nonblocking

let () = B.mysql_library_init 0 None None |> ignore

open Printf
open Util

module B = Binding_wrappers
module T = Ffi_bindings.Types(Ffi_generated_types)

module Time = Time
module Field = Field
module Row = Row
module Status = Wait_status

type t = [`Nonblocking] Common.t
type mariadb = t

type error = Common.error
type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

type server_option = Common.server_option

type 'a start = unit -> 'a result
type 'a cont = Status.t -> 'a result
type 'a nonblocking = 'a start * 'a cont

let map_result f = function
  | `Ok x -> `Ok (f x)
  | `Wait _ as w -> w
  | `Error _ as e -> e

type options =
  | Nonblocking

let options raw = function
  | Nonblocking ->
      B.mysql_options raw T.Options.nonblock Ctypes.null

let init () =
  match B.mysql_init () with
  | Some raw ->
      options raw Nonblocking;
      Some raw
  | None ->
      None

let handle_void = function
  | 0 -> `Ok
  | s -> `Wait (Status.of_int s)

let handle_opt mariadb = function
  | 0, Some _ -> `Ok mariadb
  | 0, None -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_int mariadb = function
  | 0, 0 -> `Ok ()
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_char mariadb = function
  | 0, '\000' -> `Ok ()
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let connect_start mariadb =
  let open Common in
  handle_opt mariadb
    (B.mysql_real_connect_start
      mariadb.raw
      mariadb.host
      mariadb.user
      mariadb.pass
      mariadb.db
      mariadb.port
      mariadb.socket
      mariadb.flags)

let connect_cont mariadb status =
  handle_opt mariadb
    (B.mysql_real_connect_cont mariadb.Common.raw (Status.to_int status))

let connect mariadb =
  (connect_start mariadb, connect_cont mariadb)

let close_start mariadb =
  handle_void (B.mysql_close_start mariadb.Common.raw)

let close_cont mariadb status =
  handle_void (B.mysql_close_cont mariadb.Common.raw status)

let close mariadb =
  (close_start mariadb, close_cont mariadb)

let fd mariadb =
  Obj.magic @@ B.mysql_get_socket mariadb.Common.raw

let timeout mariadb =
  B.mysql_get_timeout_value mariadb.Common.raw

let timeout_ms mariadb =
  B.mysql_get_timeout_value_ms mariadb.Common.raw

let set_character_set_start mariadb =
  let charset = Option.some mariadb.Common.charset in
  handle_int mariadb
    (B.mysql_set_character_set_start mariadb.Common.raw charset)

let set_character_set_cont mariadb status =
  handle_int mariadb (B.mysql_set_character_set_cont mariadb.Common.raw status)

let set_character_set mariadb =
  (set_character_set_start mariadb, set_character_set_cont mariadb)

let select_db_start mariadb =
  let db = Option.some mariadb.Common.db in
  handle_int mariadb (B.mysql_select_db_start mariadb.Common.raw db)

let select_db_cont mariadb status =
  handle_int mariadb (B.mysql_select_db_cont mariadb.Common.raw status)

let select_db mariadb =
  (select_db_start mariadb, select_db_cont mariadb)

let change_user_start mariadb =
  let user = Option.some mariadb.Common.user in
  let pass = Option.some mariadb.Common.pass in
  handle_char mariadb
    (B.mysql_change_user_start mariadb.Common.raw user pass mariadb.Common.db)

let change_user_cont mariadb status =
  handle_char mariadb (B.mysql_change_user_cont mariadb.Common.raw status)

let change_user mariadb =
  (change_user_start mariadb, change_user_cont mariadb)

let set_server_option_start mariadb opt =
  let opt = Common.int_of_server_option opt in
  handle_int mariadb (B.mysql_set_server_option_start mariadb.Common.raw opt)

let set_server_option_cont mariadb status =
  handle_int mariadb (B.mysql_set_server_option_cont mariadb.Common.raw status)

let set_server_option mariadb opt =
  (set_server_option_start mariadb opt, set_server_option_cont mariadb)

let ping_start mariadb =
  handle_int mariadb (B.mysql_ping_start mariadb.Common.raw)

let ping_cont mariadb status =
  handle_int mariadb (B.mysql_ping_cont mariadb.Common.raw status)

let ping mariadb =
  (ping_start mariadb, ping_cont mariadb)

let autocommit_start mariadb auto =
  handle_char mariadb (B.mysql_autocommit_start mariadb.Common.raw auto)

let autocommit_cont mariadb status =
  handle_char mariadb (B.mysql_autocommit_cont mariadb.Common.raw status)

let autocommit mariadb auto =
  (autocommit_start mariadb auto, autocommit_cont mariadb)

let commit_start mariadb =
  handle_char mariadb (B.mysql_commit_start mariadb.Common.raw)

let commit_cont mariadb status =
  handle_char mariadb (B.mysql_commit_cont mariadb.Common.raw status)

let commit mariadb =
  (commit_start mariadb, commit_cont mariadb)

let rollback_start mariadb =
  handle_char mariadb (B.mysql_rollback_start mariadb.Common.raw)

let rollback_cont mariadb status =
  handle_char mariadb (B.mysql_rollback_cont mariadb.Common.raw status)

let rollback mariadb =
  (rollback_start mariadb, rollback_cont mariadb)

let build_stmt mariadb raw =
  `Ok (Common.Stmt.init mariadb raw)

type prep_stmt =
  { raw   : B.stmt
  ; query : char Ctypes.ptr
  ; len   : int
  }

let handle_prepare mariadb stmt = function
  | 0, 0 -> build_stmt mariadb stmt.raw
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let prepare_start mariadb stmt query =
  handle_prepare mariadb stmt
    (B.mysql_stmt_prepare_start stmt.raw stmt.query stmt.len)

let prepare_cont mariadb stmt status =
  handle_prepare mariadb stmt (B.mysql_stmt_prepare_cont stmt.raw status)

let prepare mariadb query =
  match Common.stmt_init mariadb with
  | Some raw ->
      let stmt =
        { raw
        ; query = char_ptr_buffer_of_string query
        ; len   = String.length query
        } in
      `Ok (prepare_start mariadb stmt query, prepare_cont mariadb stmt)
  | None -> `Error (Common.error mariadb)

module Res = struct
  type t = [`Nonblocking] Common.Res.t

  let num_rows =
    Common.Res.num_rows

  let affected_rows =
    Common.Res.affected_rows

  let handle_fetch (type t) (module R : Row.S with type t = t) res = function
    | 0, 0 ->
        `Ok (Common.Res.build_row (module R) res)
    | 0, 1 ->
        let stmt = res.Common.Res.stmt in
        `Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)
    | 0, r when r = T.Return_code.no_data ->
        `Ok None
    | 0, r when r = T.Return_code.data_truncated ->
        `Error (2032, "truncated data")
    | s, _ ->
        `Wait (Status.of_int s)

  let fetch_start (type t) (module R : Row.S with type t = t) res =
    handle_fetch (module R) res (B.mysql_stmt_fetch_start res.Common.Res.stmt)

  let fetch_cont (type t) (module R : Row.S with type t = t) res status =
    handle_fetch (module R) res
      (B.mysql_stmt_fetch_cont res.Common.Res.stmt status)

  let fetch (type t) (module R : Row.S with type t = t) res =
    (fetch_start (module R) res, fetch_cont (module R) res)

  let handle_free res = function
    | 0, '\000' -> `Ok ()
    | 0, _ ->
        let stmt = res.Common.Res.stmt in
        `Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let free_start res =
    handle_free res (B.mysql_stmt_free_result_start res.Common.Res.stmt)

  let free_cont res status =
    handle_free res (B.mysql_stmt_free_result_cont res.Common.Res.stmt status)

  let free res =
    (free_start res, free_cont res)
end

module Stmt = struct
  type t = [`Nonblocking] Common.Stmt.t
  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

  let init =
    Common.Stmt.init

  let handle_execute stmt = function
    | 0, 0 -> `Ok stmt
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let execute_start stmt =
    handle_execute stmt (B.mysql_stmt_execute_start stmt.Common.Stmt.raw)

  let execute_cont stmt status =
    handle_execute stmt (B.mysql_stmt_execute_cont stmt.Common.Stmt.raw status)

  let execute stmt params =
    let n = B.mysql_stmt_param_count stmt.Common.Stmt.raw in
    let len = Array.length params in
    if n <> len then
      let err = sprintf "parameter count mismatch: %d (expected %d)" len n in
      `Error (2034, err)
    else
      match Common.Stmt.bind_params stmt params with
      | `Ok bound -> `Ok (execute_start bound, execute_cont bound)
      | `Error _ as err -> err

  let handle_store_result stmt = function
    | 0, 0 -> Common.Stmt.bind_result stmt
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let store_result_start stmt =
    handle_store_result stmt
      (B.mysql_stmt_store_result_start stmt.Common.Stmt.raw)

  let store_result_cont stmt status =
    handle_store_result stmt
      (B.mysql_stmt_store_result_cont stmt.Common.Stmt.raw status)

  let store_result stmt =
    (store_result_start stmt, store_result_cont stmt)

  let handle_char_unit stmt = function
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let close_start stmt =
    handle_char_unit stmt (B.mysql_stmt_close_start stmt.Common.Stmt.raw)

  let close_cont stmt status =
    handle_char_unit stmt (B.mysql_stmt_close_cont stmt.Common.Stmt.raw status)

  let close stmt =
    (close_start stmt, close_cont stmt)

  let handle_reset stmt = function
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let reset_start stmt =
    handle_reset stmt (B.mysql_stmt_reset_start stmt.Common.Stmt.raw)

  let reset_cont stmt status =
    handle_reset stmt (B.mysql_stmt_reset_cont stmt.Common.Stmt.raw status)

  let reset stmt =
    (reset_start stmt, reset_cont stmt)

  let handle_next stmt = function
    | 0, 0 -> `Ok true
    | 0, -1 -> `Ok false
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let next_result_start stmt =
    handle_next stmt (B.mysql_stmt_next_result_start stmt.Common.Stmt.raw)

  let next_result_cont stmt status =
    handle_next stmt (B.mysql_stmt_next_result_cont stmt.Common.Stmt.raw status)
end

module type Wait = sig
  module IO : sig
    type 'a future
    val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
    val return : 'a -> 'a future
  end

  val wait : t -> Status.t -> Status.t IO.future
end

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
    val fetch : (module Row.S with type t = 'r) -> t -> 'r option result future
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
  val change_user : t -> string -> string -> string option -> unit result future
  val set_client_option : t -> client_option -> unit
  val set_server_option : t -> server_option -> unit result future
  val ping : t -> unit result future
  val autocommit : t -> bool -> unit result future
  val commit : t -> unit result future
  val rollback : t -> unit result future
  val prepare : t -> string -> Stmt.t result future
end

module Make (W : Wait) : S with type 'a future = 'a W.IO.future = struct
  type t = mariadb

  type 'a future = 'a W.IO.future
  type error = int * string
  type 'a result = ('a, error) Stdlib.result

  let (>>=) = W.IO.(>>=)
  let return = W.IO.return
  let return_unit = return ()
  let (>>|) fut f = fut >>= fun x -> return (f x)

  type flag = Common.flag =
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

  type protocol = Common.protocol =
    | Default
    | Tcp
    | Socket
    | Pipe
    | Memory

  type client_option = Common.client_option =
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

  type server_option = Common.server_option =
    | Multi_statements of bool

  let rec nonblocking m (r, k) =
    match r with
    | `Ok v -> return (Ok v)
    | `Wait s -> W.wait m s >>= fun s -> nonblocking m (k s, k)
    | `Error e -> return (Error e)

  let rec nonblocking' m (r, k) =
    match r with
    | `Ok -> return_unit
    | `Wait s -> W.wait m s >>= fun s -> nonblocking' m (k s, k)

  module Time = Time
  module Field = Field
  module Row = Row

  module Res = struct
    type t = Res.t

    let fetch (type t) (module R : Row.S with type t = t) res =
      nonblocking res.Common.Res.mariadb (Res.fetch (module R) res)

    let num_rows =
      Res.num_rows

    let affected_rows =
      Res.affected_rows

    let free res =
      nonblocking res.Common.Res.mariadb (Res.free res)
  end

  module Stmt = struct
    type t = Stmt.t

    let handle_execute = function
      | Ok stmt -> nonblocking stmt.Common.Stmt.mariadb (Stmt.store_result stmt)
      | Error _ as e -> return e

    let execute stmt ps =
      match Stmt.execute stmt ps with
      | `Ok nb -> nonblocking stmt.Common.Stmt.mariadb nb >>= handle_execute
      | `Error e -> return (Error e)

    let free_res stmt =
      let handle_free = function
        | 0, '\000' -> `Ok ()
        | 0, _ -> `Error (Common.Stmt.error stmt)
        | s, _ -> `Wait (Status.of_int s) in
      let raw = stmt.Common.Stmt.raw in
      let start = handle_free (B.mysql_stmt_free_result_start raw) in
      let cont s = handle_free (B.mysql_stmt_free_result_cont raw s) in
      Common.Stmt.free_meta stmt;
      nonblocking stmt.Common.Stmt.mariadb (start, cont)

    let reset stmt =
      free_res stmt
      >>= function
      | Ok () -> nonblocking stmt.Common.Stmt.mariadb (Stmt.reset stmt)
      | Error _ as e -> return e

    let close stmt =
      free_res stmt
      >>= function
      | Ok () -> nonblocking stmt.Common.Stmt.mariadb (Stmt.close stmt)
      | Error _ as e -> return e
  end

  let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) ?(options=[]) () =
    match init () with
    | Some raw ->
        let mariadb = Common.
          { raw
          ; host    = char_ptr_opt_buffer_of_string host
          ; port    = port
          ; user    = char_ptr_opt_buffer_of_string user
          ; pass    = char_ptr_opt_buffer_of_string pass
          ; db      = char_ptr_opt_buffer_of_string db
          ; socket  = char_ptr_opt_buffer_of_string socket
          ; flags   = Common.int_of_flags flags
          ; charset = None
          } in
        List.iter (Common.set_client_option mariadb) options;
        nonblocking mariadb (connect mariadb)
    | None ->
        return (Error (2008, "out of memory"))

  let close m = nonblocking' m (close m)

  let library_end = Common.library_end

  let set_character_set m c =
    let c = Some (char_ptr_buffer_of_string c) in
    m.Common.charset <- c;
    nonblocking m (set_character_set m)

  let select_db m db =
    m.Common.db <- Some (char_ptr_buffer_of_string db);
    nonblocking m (select_db m)

  let change_user m user pass db =
    m.Common.user <- Some (char_ptr_buffer_of_string user);
    m.Common.pass <- Some (char_ptr_buffer_of_string pass);
    m.Common.db <- char_ptr_opt_buffer_of_string db;
    nonblocking m (change_user m)

  let set_client_option = Common.set_client_option

  let set_server_option m opt = nonblocking m (set_server_option m opt)

  let ping m = nonblocking m (ping m)

  let autocommit m b = nonblocking m (autocommit m b)

  let commit m = nonblocking m (commit m)

  let rollback m = nonblocking m (rollback m)

  let prepare m q =
    match prepare m q with
    | `Ok nb -> nonblocking m nb
    | `Error e -> return (Error e)
end

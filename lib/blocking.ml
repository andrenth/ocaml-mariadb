open Util

module B = Binding_wrappers
module T = Ffi_bindings.Types(Ffi_generated_types)

module Time = Time
module Field = Field
module Row = Row

type t = [`Blocking] Common.t
type mariadb = t

type error = Common.error
type 'a result = ('a, error) Stdlib.result

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

let close mariadb =
  B.mysql_close mariadb.Common.raw

let library_end = Common.library_end

let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) ?(options=[]) () =
  let flags = Common.int_of_flags flags in
  let connect raw =
    let mariadb = Common.
      { raw
      ; host    = char_ptr_opt_buffer_of_string host
      ; port    = port
      ; user    = char_ptr_opt_buffer_of_string user
      ; pass    = char_ptr_opt_buffer_of_string pass
      ; db      = char_ptr_opt_buffer_of_string db
      ; socket  = char_ptr_opt_buffer_of_string socket
      ; flags   = flags
      ; charset = None
      } in
    List.iter (Common.set_client_option mariadb) options;
    match B.mysql_real_connect raw host user pass db port socket flags with
    | Some _ -> Ok mariadb
    | None -> Error (2008, "out of memory") in
  match B.mysql_init () with
  | Some raw -> connect raw
  | None -> Error (2008, "out of memory")

let wrap_unit mariadb = function
  | true -> Ok ()
  | false -> Error (Common.error mariadb)

let set_character_set mariadb charset =
  let charset = char_ptr_buffer_of_string charset in
  mariadb.Common.charset <- Some charset;
  wrap_unit mariadb (B.mysql_set_character_set mariadb.Common.raw charset)

let select_db mariadb db =
  let db = char_ptr_buffer_of_string db in
  mariadb.Common.db <- Some db;
  wrap_unit mariadb (B.mysql_select_db mariadb.Common.raw db)

let change_user mariadb user pass db =
  let user = char_ptr_buffer_of_string user in
  let pass = char_ptr_buffer_of_string pass in
  mariadb.Common.user <- Some user;
  mariadb.Common.pass <- Some pass;
  mariadb.Common.db <- char_ptr_opt_buffer_of_string db;
  wrap_unit mariadb
    (B.mysql_change_user mariadb.Common.raw user pass mariadb.Common.db)

let set_client_option =
  Common.set_client_option

let set_server_option mariadb opt =
  let opt = Common.int_of_server_option opt in
  wrap_unit mariadb (B.mysql_set_server_option mariadb.Common.raw opt)

let ping mariadb =
  wrap_unit mariadb (B.mysql_ping mariadb.Common.raw)

let autocommit mariadb auto =
  wrap_unit mariadb (B.mysql_autocommit mariadb.Common.raw auto)

let commit mariadb =
  wrap_unit mariadb (B.mysql_commit mariadb.Common.raw)

let rollback mariadb =
  wrap_unit mariadb (B.mysql_rollback mariadb.Common.raw)

let prepare mariadb query =
  let build_stmt raw =
    if B.mysql_stmt_prepare raw query then
      Ok (Common.Stmt.init mariadb raw)
    else
      Error (Common.error mariadb) in
  match Common.stmt_init mariadb with
  | Some raw -> build_stmt raw
  | None -> Error (2008, "out of memory")

module Res = struct
  type t = [`Blocking] Common.Res.t

  let fetch (type t) (module R : Row.S with type t = t) res =
    let stmt = res.Common.Res.stmt in
    match B.mysql_stmt_fetch stmt with
    | 0 -> Ok (Common.Res.build_row (module R) res)
    | r when r = T.Return_code.no_data -> Ok None
    | r when r = T.Return_code.data_truncated -> Error (2032, "truncated data")
    | _ -> Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)

  let num_rows =
    Common.Res.num_rows

  let affected_rows =
    Common.Res.affected_rows
end

module Stmt = struct
  type t = [`Blocking] Common.Stmt.t

  let execute stmt params =
    let n = B.mysql_stmt_param_count stmt.Common.Stmt.raw in
    if n <> Array.length params then
      Error (0, "parameter count mismatch")
    else begin
      let exec stmt =
        let raw = stmt.Common.Stmt.raw in
        if B.mysql_stmt_execute raw && B.mysql_stmt_store_result raw then
          match Common.Stmt.bind_result stmt with
          | `Ok res_or_none -> Ok res_or_none
          | `Error e -> Error e
        else
          Error (Common.Stmt.error stmt) in
      match Common.Stmt.bind_params stmt params with
      | `Ok bound -> exec bound
      | `Error e -> Error e
    end

  let reset stmt =
    let raw = stmt.Common.Stmt.raw in
    if B.mysql_stmt_free_result raw && B.mysql_stmt_reset raw then
      Ok ()
    else
      Error (Common.Stmt.error stmt)

  let close stmt =
    let raw = stmt.Common.Stmt.raw in
    if B.mysql_stmt_free_result raw && B.mysql_stmt_close raw then
      Ok ()
    else
      Error (Common.Stmt.error stmt)
end

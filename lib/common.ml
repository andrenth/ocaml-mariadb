open Util

module B = Binding_wrappers
module T = Ffi_bindings.Types(Ffi_generated_types)

module Row = Row
module Field = Field

type mode = [`Blocking | `Nonblocking]
type 'm t =
  { raw             : B.mysql
  ; host            : char Ctypes.ptr option
  ; port            : int
  ; mutable user    : char Ctypes.ptr option
  ; mutable pass    : char Ctypes.ptr option
  ; mutable db      : char Ctypes.ptr option
  ; socket          : char Ctypes.ptr option
  ; flags           : int32
  ; mutable charset : char Ctypes.ptr option
  }
  constraint 'm = [< mode]

type 'm mariadb = 'm t

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

type error = int * string

let error mariadb =
  (B.mysql_errno mariadb.raw, B.mysql_error mariadb.raw)

let int_of_server_option = function
  | Multi_statements true -> T.Server_options.multi_statements_on
  | Multi_statements false -> T.Server_options.multi_statements_off

let voidp_of_string s =
  let open Ctypes in
  let b = char_ptr_buffer_of_string s in
  coerce (ptr char) (ptr void) b

let voidp_of_uint i =
  let open Ctypes in
  let b = allocate uint (Unsigned.UInt.of_int i) in
  coerce (ptr uint) (ptr void) b

let voidp_of_bool b =
  let open Ctypes in
  let b = allocate char (if b then '\001' else '\000') in
  coerce (ptr char) (ptr void) b

let int_of_protocol = function
  | Default -> T.Protocol.default
  | Tcp -> T.Protocol.tcp
  | Socket -> T.Protocol.socket
  | Pipe -> T.Protocol.pipe
  | Memory -> T.Protocol.memory

let set_client_option mariadb opt =
  let opt =
    match opt with
    | Connect_timeout t ->
        `Opt (T.Options.connect_timeout, voidp_of_uint t)
    | Compress ->
        `Opt (T.Options.compress, Ctypes.null)
    | Named_pipe pipe ->
        `Opt (T.Options.named_pipe, voidp_of_string pipe)
    | Init_command cmd ->
        `Opt (T.Options.init_command, voidp_of_string cmd)
    | Read_default_file file ->
        `Opt (T.Options.read_default_file, voidp_of_string file)
    | Read_default_group group ->
        `Opt (T.Options.read_default_group, voidp_of_string group)
    | Set_charset_dir dir ->
        `Opt (T.Options.set_charset_dir, voidp_of_string dir)
    | Set_charset_name name ->
        `Opt (T.Options.set_charset_name, voidp_of_string name)
    | Local_infile b ->
        `Opt (T.Options.local_infile, voidp_of_uint (if b then 1 else 0))
    | Protocol proto ->
        `Opt (T.Options.protocol, voidp_of_uint (int_of_protocol proto))
    | Shared_memory_base_name name ->
        `Opt (T.Options.shared_memory_base_name, voidp_of_string name)
    | Read_timeout t ->
        `Opt (T.Options.read_timeout, voidp_of_uint t)
    | Write_timeout t ->
        `Opt (T.Options.write_timeout, voidp_of_uint t)
    | Secure_auth b ->
        `Opt (T.Options.secure_auth, voidp_of_bool b)
    | Report_data_truncation b ->
        `Opt (T.Options.report_data_truncation, voidp_of_bool b)
    | Reconnect b ->
        `Opt (T.Options.reconnect, voidp_of_bool b)
    | Ssl_verify_server_cert b ->
        `Opt (T.Options.ssl_verify_server_cert, voidp_of_bool b)
    | Plugin_dir dir ->
        `Opt (T.Options.plugin_dir, voidp_of_string dir)
    | Default_auth auth ->
        `Opt (T.Options.default_auth, voidp_of_string auth)
    | Bind addr ->
        `Opt (T.Options.bind, voidp_of_string addr)
    | Ssl_key key ->
        `Opt (T.Options.ssl_key, voidp_of_string key)
    | Ssl_cert cert ->
        `Opt (T.Options.ssl_cert, voidp_of_string cert)
    | Ssl_ca ca ->
        `Opt (T.Options.ssl_ca, voidp_of_string ca)
    | Ssl_capath path ->
        `Opt (T.Options.ssl_capath, voidp_of_string path)
    | Ssl_cipher cipher ->
        `Opt (T.Options.ssl_cipher, voidp_of_string cipher)
    | Ssl_crl crl ->
        `Opt (T.Options.ssl_crl, voidp_of_string crl)
    | Ssl_crlpath path ->
        `Opt (T.Options.ssl_crlpath, voidp_of_string path)
    | Connect_attr_reset ->
        `Opt (T.Options.connect_attr_reset, Ctypes.null)
    | Connect_attr_add (k, v) ->
        `Opt4 (T.Options.connect_attr_add, voidp_of_string k, voidp_of_string v)
    | Connect_attr_delete attr ->
        `Opt (T.Options.connect_attr_delete, voidp_of_string attr)
    | Server_public_key key ->
        `Opt (T.Options.server_public_key, voidp_of_string key)
    | Enable_cleartext_plugin b ->
        `Opt (T.Options.enable_cleartext_plugin, voidp_of_bool b) in
  match opt with
  | `Opt (opt, arg) -> B.mysql_options mariadb.raw opt arg
  | `Opt4 (opt, arg1, arg2) -> B.mysql_options4 mariadb.raw opt arg1 arg2

let int_of_flag = function
  | Found_rows -> T.Flags.found_rows
  | Compress -> T.Flags.compress
  | Ignore_sigpipe -> T.Flags.ignore_sigpipe
  | Ignore_space -> T.Flags.ignore_space
  | Interactive -> T.Flags.interactive
  | Local_files -> T.Flags.local_files
  | Multi_results -> T.Flags.multi_results
  | Multi_statements -> T.Flags.multi_statements
  | No_schema -> T.Flags.no_schema
  | Odbc -> T.Flags.odbc
  | Ssl -> T.Flags.ssl
  | Remember_options -> T.Flags.remember_options

let int_of_flags =
  List.fold_left (fun acc flag -> Int32.logor acc (int_of_flag flag)) 0l

module Res = struct
  open Ctypes

  type meta =
    { result  : Bind.t
    ; raw     : B.res
    ; buffers : unit ptr array
    }

  type 'm u =
    { mariadb : 'm mariadb
    ; stmt    : B.stmt
    ; meta    : meta option
    }
  type 'm t = 'm u constraint 'm = [< mode]

  let meta result raw buffers =
    { result; raw; buffers }

  let create ~mariadb ~stmt ?meta () =
    { mariadb; stmt; meta }

  let num_rows res =
    B.mysql_stmt_num_rows res.stmt

  let affected_rows res =
    B.mysql_stmt_affected_rows res.stmt

  let fetch_field raw i =
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct raw i)

  let build_row (type t) (module R : Row.S with type t = t) res =
    Option.map
      (fun {result; raw; _} ->
        R.build result.Bind.n
          (fun i ->
            let fp = fetch_field raw i in
            Field.create result fp i))
      res.meta
end

let stmt_init mariadb =
  match B.mysql_stmt_init mariadb.raw with
  | Some stmt ->
      B.mysql_stmt_attr_set_bool stmt T.Stmt_attr.update_max_length true;
      Some stmt
  | None ->
      None

let library_end () =
  B.mysql_library_end ()

module Stmt = struct
  open Ctypes

  type meta =
    { res : B.res
    ; result : Bind.t
    }

  type 'm u =
    { raw : B.stmt
    ; mariadb : 'm mariadb
    ; num_params : int
    ; params : Bind.t
    ; mutable meta : meta option
    }
  type 'm t = 'm u constraint 'm = [< mode]

  type cursor_type
    = No_cursor
    | Read_only

  type attr
    = Update_max_length of bool
    | Cursor_type of cursor_type
    | Prefetch_rows of int

  let error stmt =
    (B.mysql_stmt_errno stmt.raw, B.mysql_stmt_error stmt.raw)

  let fetch_field res i =
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res i)

  let test_unsigned flags =
    Unsigned.UInt.logand flags T.Field.Flags.unsigned <> Unsigned.UInt.zero

  let alloc_result res n =
    let r = Bind.alloc n in
    for i = 0 to n - 1 do
      let bp = r.Bind.bind +@ i in
      let fp = fetch_field res i in
      let flags = getf (!@fp) T.Field.flags in
      let is_unsigned = if test_unsigned flags then '\001' else '\000' in
      setf (!@bp) T.Bind.buffer_type (getf (!@fp) T.Field.typ);
      setf (!@bp) T.Bind.length (r.Bind.length +@ i);
      setf (!@bp) T.Bind.is_null (r.Bind.is_null +@ i);
      setf (!@bp) T.Bind.is_unsigned is_unsigned;
      setf (!@bp) T.Bind.error (r.Bind.error +@ i)
    done;
    r

  let init mariadb raw =
    let np = B.mysql_stmt_param_count raw in
    { raw
    ; mariadb
    ; num_params = np
    ; params = Bind.alloc np
    ; meta = None
    }

  let bind_params stmt params =
    match Array.length params with
    | 0 -> `Ok stmt
    | n ->
        let b = stmt.params in
        Array.iteri
          (fun at arg ->
            match arg with
            | `Null -> Bind.null b ~at
            | `Int i -> Bind.int b i ~at
            | `Float x -> Bind.float b x ~at
            | `String s -> Bind.string b s ~at
            | `Bytes s -> Bind.blob b s ~at
            | `Time t -> Bind.time b t ~at)
          params;
        if B.mysql_stmt_bind_param stmt.raw b.Bind.bind then
          `Ok stmt
        else
          `Error (error stmt)

  (* From http://dev.mysql.com/doc/refman/5.7/en/mysql-stmt-fetch.html *)
  let buffer_size typ =
    match Bind.buffer_type_of_int typ with
    | `Null -> 0
    | `Tiny | `Year -> 1
    | `Short -> 2
    | `Int24 | `Long | `Float -> 4
    | `Long_long | `Double -> 8
    | `Decimal | `New_decimal | `String | `Var_string
    | `Tiny_blob | `Blob | `Medium_blob | `Long_blob | `Bit -> -1
    | `Time | `Date | `Datetime | `Timestamp -> Ctypes.sizeof T.Time.t

  let malloc count =
    let p = allocate_n char ~count in
    coerce (ptr char) (ptr void) p

  let alloc_buffer b fp i =
    let bp = b.Bind.bind +@ i in
    let typ = getf (!@bp) T.Bind.buffer_type in
    match buffer_size typ with
    | -1 ->
        let n = getf (!@fp) T.Field.max_length in
        setf (!@bp) T.Bind.buffer_length n;
        b.Bind.buffers.(i) <- malloc (Unsigned.ULong.to_int n);
        setf (!@bp) T.Bind.buffer b.Bind.buffers.(i)
    | n ->
        setf (!@bp) T.Bind.buffer_length (Unsigned.ULong.of_int n);
        b.Bind.buffers.(i) <- malloc n;
        setf (!@bp) T.Bind.buffer b.Bind.buffers.(i)

  let free_meta stmt =
    match stmt.meta with
    | Some { res; _ } ->
        B.mysql_free_result res;
        stmt.meta <- None
    | None -> ()

  let meta stmt =
    free_meta stmt;
    stmt.meta <- (
        match B.mysql_stmt_result_metadata stmt.raw with
        | Some res ->
            let nf = B.mysql_num_fields res in
            Some
              { res
              ; result = alloc_result res nf
              }
        | None -> None);
    stmt.meta

  let bind_result stmt =
    match meta stmt with
    | Some meta ->
        let b = meta.result in
        let n = b.Bind.n in
        for i = 0 to n - 1 do
          let fp = fetch_field meta.res i in
          alloc_buffer b fp i
        done;
        if B.mysql_stmt_bind_result stmt.raw meta.result.Bind.bind then
          let meta = Res.meta meta.result meta.res b.Bind.buffers in
          let res =
            Res.create
              ~mariadb:stmt.mariadb
              ~stmt:stmt.raw
              ~meta () in
          `Ok res
        else
          `Error (error stmt)
    | None ->
        `Ok (Res.create ~mariadb:stmt.mariadb ~stmt:stmt.raw ())
end

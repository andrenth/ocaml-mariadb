open Ctypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  module Protocol = struct
    let default = constant "MYSQL_PROTOCOL_DEFAULT" int
    let tcp = constant "MYSQL_PROTOCOL_TCP" int
    let socket = constant "MYSQL_PROTOCOL_SOCKET" int
    let pipe = constant "MYSQL_PROTOCOL_PIPE" int
    let memory = constant "MYSQL_PROTOCOL_MEMORY" int
  end

  module Options = struct
    let connect_timeout = constant "MYSQL_OPT_CONNECT_TIMEOUT" int
    let compress = constant "MYSQL_OPT_COMPRESS" int
    let named_pipe = constant "MYSQL_OPT_NAMED_PIPE" int
    let init_command = constant "MYSQL_INIT_COMMAND" int
    let read_default_file = constant "MYSQL_READ_DEFAULT_FILE" int
    let read_default_group = constant "MYSQL_READ_DEFAULT_GROUP" int
    let set_charset_dir = constant "MYSQL_SET_CHARSET_DIR" int
    let set_charset_name = constant "MYSQL_SET_CHARSET_NAME" int
    let local_infile = constant "MYSQL_OPT_LOCAL_INFILE" int
    let protocol = constant "MYSQL_OPT_PROTOCOL" int
    let shared_memory_base_name = constant "MYSQL_SHARED_MEMORY_BASE_NAME" int
    let read_timeout = constant "MYSQL_OPT_READ_TIMEOUT" int
    let write_timeout = constant "MYSQL_OPT_WRITE_TIMEOUT" int
    let secure_auth = constant "MYSQL_SECURE_AUTH" int
    let report_data_truncation = constant "MYSQL_REPORT_DATA_TRUNCATION" int
    let reconnect = constant "MYSQL_OPT_RECONNECT" int
    let ssl_verify_server_cert = constant "MYSQL_OPT_SSL_VERIFY_SERVER_CERT" int
    let plugin_dir = constant "MYSQL_PLUGIN_DIR" int
    let default_auth = constant "MYSQL_DEFAULT_AUTH" int
    let bind = constant "MYSQL_OPT_BIND" int
    let ssl_key = constant "MYSQL_OPT_SSL_KEY" int
    let ssl_cert = constant "MYSQL_OPT_SSL_CERT" int
    let ssl_ca = constant "MYSQL_OPT_SSL_CA" int
    let ssl_capath = constant "MYSQL_OPT_SSL_CAPATH" int
    let ssl_cipher = constant "MYSQL_OPT_SSL_CIPHER" int
    let ssl_crl = constant "MYSQL_OPT_SSL_CRL" int
    let ssl_crlpath = constant "MYSQL_OPT_SSL_CRLPATH" int
    let connect_attr_reset = constant "MYSQL_OPT_CONNECT_ATTR_RESET" int
    let connect_attr_add = constant "MYSQL_OPT_CONNECT_ATTR_ADD" int
    let connect_attr_delete = constant "MYSQL_OPT_CONNECT_ATTR_DELETE" int
    let server_public_key = constant "MYSQL_SERVER_PUBLIC_KEY" int
    let enable_cleartext_plugin = constant "MYSQL_ENABLE_CLEARTEXT_PLUGIN" int
    let nonblock = constant "MYSQL_OPT_NONBLOCK" int
  end

  module Flags = struct
    let compress = constant "CLIENT_COMPRESS" int32_t
    let found_rows = constant "CLIENT_FOUND_ROWS" int32_t
    let ignore_sigpipe = constant "CLIENT_IGNORE_SIGPIPE" int32_t
    let ignore_space = constant "CLIENT_IGNORE_SPACE" int32_t
    let interactive = constant "CLIENT_INTERACTIVE" int32_t
    let local_files = constant "CLIENT_LOCAL_FILES" int32_t
    let multi_results = constant "CLIENT_MULTI_RESULTS" int32_t
    let multi_statements = constant "CLIENT_MULTI_STATEMENTS" int32_t
    let no_schema = constant "CLIENT_NO_SCHEMA" int32_t
    let odbc = constant "CLIENT_ODBC" int32_t
    let ssl = constant "CLIENT_SSL" int32_t
    let remember_options = constant "CLIENT_REMEMBER_OPTIONS" int32_t
  end

  module Server_options = struct
    let multi_statements_on = constant "MYSQL_OPTION_MULTI_STATEMENTS_ON" int
    let multi_statements_off = constant "MYSQL_OPTION_MULTI_STATEMENTS_OFF" int
  end

  module Wait_status = struct
    let read = constant "MYSQL_WAIT_READ" int
    let write = constant "MYSQL_WAIT_WRITE" int
    let except = constant "MYSQL_WAIT_EXCEPT" int
    let timeout = constant "MYSQL_WAIT_TIMEOUT" int
  end

  module Type = struct
    let null = constant "MYSQL_TYPE_NULL" int
    let tiny = constant "MYSQL_TYPE_TINY" int
    let year = constant "MYSQL_TYPE_YEAR" int
    let short = constant "MYSQL_TYPE_SHORT" int
    let int24 = constant "MYSQL_TYPE_INT24" int
    let long = constant "MYSQL_TYPE_LONG" int
    let float = constant "MYSQL_TYPE_FLOAT" int
    let long_long = constant "MYSQL_TYPE_LONGLONG" int
    let double = constant "MYSQL_TYPE_DOUBLE" int
    let decimal = constant "MYSQL_TYPE_DECIMAL" int
    let new_decimal = constant "MYSQL_TYPE_NEWDECIMAL" int
    let string = constant "MYSQL_TYPE_STRING" int
    let var_string = constant "MYSQL_TYPE_VAR_STRING" int
    let tiny_blob = constant "MYSQL_TYPE_TINY_BLOB" int
    let blob = constant "MYSQL_TYPE_BLOB" int
    let medium_blob = constant "MYSQL_TYPE_MEDIUM_BLOB" int
    let long_blob = constant "MYSQL_TYPE_LONG_BLOB" int
    let bit = constant "MYSQL_TYPE_BIT" int
    let time = constant "MYSQL_TYPE_TIME" int
    let date = constant "MYSQL_TYPE_DATE" int
    let datetime = constant "MYSQL_TYPE_DATETIME" int
    let timestamp = constant "MYSQL_TYPE_TIMESTAMP" int
  end

  module Stmt_attr = struct
    let update_max_length = constant "STMT_ATTR_UPDATE_MAX_LENGTH" int
    let cursor_type = constant "STMT_ATTR_CURSOR_TYPE" int
    let prefetch_rows = constant "STMT_ATTR_PREFETCH_ROWS" int
  end

  module Return_code = struct
    let no_data = constant "MYSQL_NO_DATA" int
    let data_truncated = constant "MYSQL_DATA_TRUNCATED" int
  end

  module Bind = struct
    type bind
    type t = bind structure
    let t : t typ = structure "st_mysql_bind"

    let length = field t "length" (ptr ulong)
    let is_null = field t "is_null" (ptr char)
    let buffer = field t "buffer" (ptr void)
    let error = field t "error" (ptr char)
    let buffer_length = field t "buffer_length" ulong
    let buffer_type = field t "buffer_type" int
    let is_unsigned = field t "is_unsigned" char

    let () = seal t
  end

  module Field = struct
    type field
    type t = field structure
    let t : t typ = structure "st_mysql_field"

    let name = field t "name" string
    let max_length = field t "max_length" ulong
    let flags = field t "flags" uint
    let typ = field t "type" int

    let () = seal t

    module Flags = struct
      let not_null = constant "NOT_NULL_FLAG" uint
      let unsigned = constant "UNSIGNED_FLAG" uint
    end
  end

  module Time = struct
    type time
    type t = time structure
    let t : t typ = structure "st_mysql_time"

    let year = field t "year" uint
    let month = field t "month" uint
    let day = field t "day" uint
    let hour = field t "hour" uint
    let minute = field t "minute" uint
    let second = field t "second" uint
    let second_part = field t "second_part" ulong

    let () = seal t
  end
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  type st_mysql
  let st_mysql : st_mysql structure typ = structure "st_mysql"

  type mysql = st_mysql structure ptr
  let mysql : mysql typ = ptr st_mysql

  type mysql_opt = st_mysql structure ptr option
  let mysql_opt : mysql_opt typ = ptr_opt st_mysql

  type st_mysql_res
  let st_mysql_res : st_mysql_res structure typ = structure "st_mysql_res"

  type res = st_mysql_res structure ptr
  let res : res typ = ptr st_mysql_res

  type res_opt = st_mysql_res structure ptr option
  let res_opt : res_opt typ = ptr_opt st_mysql_res

  type row = char ptr ptr
  let row : row typ = ptr (ptr char)

  type row_opt = char ptr ptr option
  let row_opt : row_opt typ = ptr_opt (ptr char)

  type st_mysql_stmt
  let st_mysql_stmt : st_mysql_stmt structure typ = structure "st_mysql_stmt"

  type stmt = st_mysql_stmt structure ptr
  let stmt : stmt typ = ptr st_mysql_stmt

  type stmt_opt = st_mysql_stmt structure ptr option
  let stmt_opt : stmt_opt typ = ptr_opt st_mysql_stmt

  type st_mysql_field
  let st_mysql_field : st_mysql_field structure typ = structure "st_mysql_field"

  type field = st_mysql_field structure ptr
  let field : field typ = ptr st_mysql_field

  type my_bool = char
  let my_bool : char typ = char

  (* Common API *)

  let mysql_library_init = foreign "mysql_server_init"
    (int @-> ptr_opt (ptr char) @-> ptr_opt (ptr char) @-> returning int)

  let mysql_library_end = foreign "mysql_server_end"
    (void @-> returning void)

  let mysql_init = foreign "mysql_init"
    (mysql_opt @-> returning mysql_opt)

  let mysql_close = foreign "mysql_close"
    (mysql @-> returning void)

  let mysql_options = foreign "mysql_options"
    (mysql @-> int @-> ptr void @-> returning int)

  let mysql_options4 = foreign "mysql_options4"
    (mysql @-> int @-> ptr void @-> ptr void @-> returning int)

  let mysql_num_fields = foreign "mysql_num_fields"
    (res @-> returning int)

  let mysql_errno = foreign "mysql_errno"
    (mysql @-> returning int)

  let mysql_error = foreign "mysql_error"
    (mysql @-> returning string)

  let mysql_stmt_init = foreign "mysql_stmt_init"
    (mysql @-> returning stmt_opt)

  let mysql_stmt_errno = foreign "mysql_stmt_errno"
    (stmt @-> returning int)

  let mysql_stmt_error = foreign "mysql_stmt_error"
    (stmt @-> returning string)

  let mysql_stmt_attr_set = foreign "mysql_stmt_attr_set"
    (stmt @-> int @-> ptr void @-> returning my_bool)

  (* XXX ptr void because we can't access Bind.t here *)
  let mysql_stmt_bind_param = foreign "mysql_stmt_bind_param"
    (stmt @-> ptr void @-> returning my_bool)

  let mysql_stmt_param_count = foreign "mysql_stmt_param_count"
    (stmt @-> returning ulong)

  let mysql_stmt_result_metadata = foreign "mysql_stmt_result_metadata"
    (stmt @-> returning res_opt)

  (* XXX ptr void because we can't access Field.t here *)
  let mysql_fetch_field_direct = foreign "mysql_fetch_field_direct"
    (res @-> uint @-> returning (ptr void))

  (* XXX ptr void because we can't access Bind.t here *)
  let mysql_stmt_bind_result = foreign "mysql_stmt_bind_result"
    (stmt @-> ptr void @-> returning my_bool)

  let mysql_stmt_num_rows = foreign "mysql_stmt_num_rows"
    (stmt @-> returning ullong)

  let mysql_stmt_affected_rows = foreign "mysql_stmt_affected_rows"
    (stmt @-> returning ullong)

  (* Blocking API *)

  let mysql_free_result = foreign "mysql_free_result"
    (res @-> returning void)

  let mysql_real_connect = foreign "mysql_real_connect"
    (mysql @-> ptr_opt char @-> ptr_opt char @->
     ptr_opt char @-> ptr_opt char @-> uint @-> ptr_opt char @-> ulong @->
     returning mysql_opt)

  let mysql_commit = foreign "mysql_commit"
    (mysql @-> returning my_bool)

  let mysql_rollback = foreign "mysql_rollback"
    (mysql @-> returning my_bool)

  let mysql_autocommit = foreign "mysql_autocommit"
    (mysql @-> my_bool @-> returning my_bool)

  let mysql_set_character_set = foreign "mysql_set_character_set"
    (mysql @-> ptr char @-> returning int)

  let mysql_select_db = foreign "mysql_select_db"
    (mysql @-> ptr char @-> returning int)

  let mysql_change_user = foreign "mysql_change_user"
    (mysql @-> ptr char @-> ptr char @-> ptr_opt char @-> returning my_bool)

  let mysql_set_server_option = foreign "mysql_set_server_option"
    (mysql @-> int @-> returning int)

  let mysql_ping = foreign "mysql_ping"
    (mysql @-> returning int)

  let mysql_stmt_prepare = foreign "mysql_stmt_prepare"
    (stmt @-> ptr char @-> ulong @-> returning int)

  let mysql_stmt_reset = foreign "mysql_stmt_reset"
    (stmt @-> returning my_bool)

  let mysql_stmt_execute = foreign "mysql_stmt_execute"
    (stmt @-> returning int)

  let mysql_stmt_fetch = foreign "mysql_stmt_fetch"
    (stmt @-> returning int)

  let mysql_stmt_close = foreign "mysql_stmt_close"
    (stmt @-> returning my_bool)

  let mysql_stmt_store_result = foreign "mysql_stmt_store_result"
    (stmt @-> returning int)

  let mysql_stmt_free_result = foreign "mysql_stmt_free_result"
    (stmt @-> returning my_bool)

  (* Nonblocking API *)

  let mysql_close_start = foreign "mysql_close_start"
    (mysql @-> returning int)

  let mysql_close_cont = foreign "mysql_close_cont"
    (mysql @-> int @-> returning int)

  let mysql_real_connect_start = foreign "mysql_real_connect_start"
    (ptr mysql_opt @-> mysql @-> ptr_opt char @-> ptr_opt char @->
     ptr_opt char @-> ptr_opt char @-> uint @-> ptr_opt char @-> ulong @->
     returning int)

  let mysql_real_connect_cont = foreign "mysql_real_connect_cont"
    (ptr mysql_opt @-> mysql @-> int @-> returning int)

  let mysql_get_socket = foreign "mysql_get_socket"
    (mysql @-> returning int)

  let mysql_get_timeout_value = foreign "mysql_get_timeout_value"
    (mysql @-> returning uint)

  let mysql_get_timeout_value_ms = foreign "mysql_get_timeout_value_ms"
    (mysql @-> returning uint)

  let mysql_set_character_set_start = foreign "mysql_set_character_set_start"
    (ptr int @-> mysql @-> ptr char @-> returning int)

  let mysql_set_character_set_cont = foreign "mysql_set_character_set_cont"
    (ptr int @-> mysql @-> int @-> returning int)

  let mysql_select_db_start = foreign "mysql_select_db_start"
    (ptr int @-> mysql @-> ptr char @-> returning int)

  let mysql_select_db_cont = foreign "mysql_select_db_cont"
    (ptr int @-> mysql @-> int @-> returning int)

  let mysql_change_user_start = foreign "mysql_change_user_start"
    (ptr my_bool @-> mysql @-> ptr char @-> ptr char @-> ptr_opt char @->
     returning int)

  let mysql_change_user_cont = foreign "mysql_change_user_cont"
    (ptr my_bool @-> mysql @-> int @-> returning int)

  let mysql_set_server_option_start = foreign "mysql_set_server_option_start"
    (ptr int @-> mysql @-> int @-> returning int)

  let mysql_set_server_option_cont = foreign "mysql_set_server_option_cont"
    (ptr int @-> mysql @-> int @-> returning int)

  let mysql_ping_start = foreign "mysql_ping_start"
    (ptr int @-> mysql @-> returning int)

  let mysql_ping_cont = foreign "mysql_ping_cont"
    (ptr int @-> mysql @-> int @-> returning int)

  let mysql_stmt_prepare_start = foreign "mysql_stmt_prepare_start"
    (ptr int @-> stmt @-> ptr char @-> ulong @-> returning int)

  let mysql_stmt_prepare_cont = foreign "mysql_stmt_prepare_cont"
    (ptr int @-> stmt @-> int @-> returning int)

  let mysql_stmt_reset_start = foreign "mysql_stmt_reset_start"
    (ptr my_bool @-> stmt @-> returning int)

  let mysql_stmt_reset_cont = foreign "mysql_stmt_reset_cont"
    (ptr my_bool @-> stmt @-> int @-> returning int)

  let mysql_stmt_execute_start = foreign "mysql_stmt_execute_start"
    (ptr int @-> stmt @-> returning int)

  let mysql_stmt_execute_cont = foreign "mysql_stmt_execute_cont"
    (ptr int @-> stmt @-> int @-> returning int)

  let mysql_stmt_fetch_start = foreign "mysql_stmt_fetch_start"
    (ptr int @-> stmt @-> returning int)

  let mysql_stmt_fetch_cont = foreign "mysql_stmt_fetch_cont"
    (ptr int @-> stmt @-> int @-> returning int)

  let mysql_stmt_store_result_start = foreign "mysql_stmt_store_result_start"
    (ptr int @-> stmt @-> returning int)

  let mysql_stmt_store_result_cont = foreign "mysql_stmt_store_result_cont"
    (ptr int @-> stmt @-> int @-> returning int)

  let mysql_stmt_close_start = foreign "mysql_stmt_close_start"
    (ptr my_bool @-> stmt @-> returning int)

  let mysql_stmt_close_cont = foreign "mysql_stmt_close_cont"
    (ptr my_bool @-> stmt @-> int @-> returning int)

  let mysql_stmt_free_result_start = foreign "mysql_stmt_free_result_start"
    (ptr my_bool @-> stmt @-> returning int)

  let mysql_stmt_free_result_cont = foreign "mysql_stmt_free_result_cont"
    (ptr my_bool @-> stmt @-> int @-> returning int)

  let mysql_commit_start = foreign "mysql_commit_start"
    (ptr my_bool @-> mysql @-> returning int)

  let mysql_commit_cont = foreign "mysql_commit_cont"
    (ptr my_bool @-> mysql @-> int @-> returning int)

  let mysql_rollback_start = foreign "mysql_rollback_start"
    (ptr my_bool @-> mysql @-> returning int)

  let mysql_rollback_cont = foreign "mysql_rollback_cont"
    (ptr my_bool @-> mysql @-> int @-> returning int)

  let mysql_autocommit_start = foreign "mysql_autocommit_start"
    (ptr my_bool @-> mysql @-> my_bool @-> returning int)

  let mysql_autocommit_cont = foreign "mysql_autocommit_cont"
    (ptr my_bool @-> mysql @-> int @-> returning int)

  let mysql_stmt_next_result_start = foreign "mysql_stmt_next_result_start"
    (ptr int @-> stmt @-> returning int)

  let mysql_stmt_next_result_cont = foreign "mysql_stmt_next_result_cont"
    (ptr int @-> stmt @-> int @-> returning int)
end

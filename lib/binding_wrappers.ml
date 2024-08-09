open Ctypes
open Util

module B = Ffi_generated.Functions
module T = Ffi_generated.Types

include B

let handle (typ, z) f =
  let r = allocate typ z in
  let s = f r in
  (s, !@r)

let handle_opt typ = handle (typ, None)
let handle_int f = handle (int, 0) f
let handle_char f = handle (char, '\000') f
let handle_ret = handle_opt B.mysql_opt

let mysql_init () =
  B.mysql_init None

let mysql_options mysql opt value =
  B.mysql_options mysql opt value |> ignore

let mysql_options4 mysql opt value1 value2 =
  B.mysql_options4 mysql opt value1 value2 |> ignore

let mysql_stmt_attr_set_bool stmt attr value =
  let c = if value then '\001' else '\000' in
  let v = allocate B.my_bool c in
  B.mysql_stmt_attr_set stmt attr (to_voidp v) |> ignore

let mysql_stmt_param_count stmt =
  Unsigned.ULong.to_int @@ B.mysql_stmt_param_count stmt

let mysql_stmt_bind_param stmt bind =
  B.mysql_stmt_bind_param stmt (to_voidp bind) = '\000'

let mysql_fetch_field_direct res i =
  B.mysql_fetch_field_direct res (Unsigned.UInt.of_int i)

let mysql_stmt_bind_result stmt bind =
  B.mysql_stmt_bind_result stmt (to_voidp bind) = '\000'

let mysql_stmt_num_rows stmt =
  Unsigned.ULLong.to_int @@ B.mysql_stmt_num_rows stmt

let mysql_stmt_affected_rows stmt =
  Unsigned.ULLong.to_int @@ B.mysql_stmt_affected_rows stmt

(* Blocking API *)

let mysql_real_connect mysql host user pass db port socket flags =
  let host = char_ptr_opt_buffer_of_string host in
  let user = char_ptr_opt_buffer_of_string user in
  let pass = char_ptr_opt_buffer_of_string pass in
  let db = char_ptr_opt_buffer_of_string db in
  let port = Unsigned.UInt.of_int port in
  let socket = char_ptr_opt_buffer_of_string socket in
  let flags = Unsigned.ULong.of_int64 (Int64.of_int32 flags) in
  B.mysql_real_connect mysql host user pass db port socket flags

let mysql_commit mysql =
  B.mysql_commit mysql = '\000'

let mysql_rollback mysql =
  B.mysql_rollback mysql = '\000'

let mysql_autocommit mysql auto =
  let auto = if auto then '\001' else '\000' in
  B.mysql_autocommit mysql auto = '\000'

let mysql_set_character_set mysql charset =
  B.mysql_set_character_set mysql charset = 0

let mysql_select_db mysql db =
  B.mysql_select_db mysql db = 0

let mysql_change_user mysql user pass db =
  B.mysql_change_user mysql user pass db = '\000'

let mysql_set_server_option mysql opt =
  B.mysql_set_server_option mysql opt = 0

let mysql_ping mysql =
  B.mysql_ping mysql = 0

let mysql_stmt_prepare stmt query =
  let len = Unsigned.ULong.of_int (String.length query) in
  let query = char_ptr_buffer_of_string query in
  B.mysql_stmt_prepare stmt query len = 0

let mysql_stmt_reset stmt =
  B.mysql_stmt_reset stmt = '\000'

let mysql_stmt_execute stmt =
  B.mysql_stmt_execute stmt = 0

let mysql_stmt_close stmt =
  B.mysql_stmt_close stmt = '\000'

let mysql_stmt_store_result stmt =
  B.mysql_stmt_store_result stmt = 0

let mysql_stmt_free_result stmt =
  B.mysql_stmt_free_result stmt = '\000'

(* Nonblocking API *)

let mysql_real_connect_start mysql host user pass db port socket flags =
  let port = Unsigned.UInt.of_int port in
  let flags = Unsigned.ULong.of_int64 (Int64.of_int32 flags) in
  handle_ret
    (fun ret ->
      B.mysql_real_connect_start ret mysql host user pass db port socket flags)

let mysql_real_connect_cont mysql status =
  handle_ret (fun ret -> B.mysql_real_connect_cont ret mysql status)

let mysql_get_timeout_value mysql =
  Unsigned.UInt.to_int @@ B.mysql_get_timeout_value mysql

let mysql_get_timeout_value_ms mysql =
  Unsigned.UInt.to_int @@ B.mysql_get_timeout_value_ms mysql

let mysql_set_character_set_start mysql charset =
  handle_int (fun ret -> B.mysql_set_character_set_start ret mysql charset)

let mysql_set_character_set_cont mysql status =
  handle_int (fun ret -> B.mysql_set_character_set_cont ret mysql status)

let mysql_select_db_start mysql db =
  handle_int (fun ret -> B.mysql_select_db_start ret mysql db)

let mysql_select_db_cont mysql status =
  handle_int (fun ret -> B.mysql_select_db_cont ret mysql status)

let mysql_change_user_start mysql user pass db =
  handle_char (fun ret -> B.mysql_change_user_start ret mysql user pass db)

let mysql_change_user_cont mysql status =
  handle_char (fun ret -> B.mysql_change_user_cont ret mysql status)

let mysql_set_server_option_start mysql opt =
  handle_int (fun ret -> B.mysql_set_server_option_start ret mysql opt)

let mysql_set_server_option_cont mysql status =
  handle_int (fun ret -> B.mysql_set_server_option_cont ret mysql status)

let mysql_ping_start mysql =
  handle_int (fun ret -> B.mysql_ping_start ret mysql)

let mysql_ping_cont mysql status =
  handle_int (fun ret -> B.mysql_ping_cont ret mysql status)

let mysql_stmt_prepare_start stmt query len =
  let len = Unsigned.ULong.of_int len in
  handle_int (fun err -> B.mysql_stmt_prepare_start err stmt query len)

let mysql_stmt_prepare_cont stmt status =
  handle_int (fun err -> B.mysql_stmt_prepare_cont err stmt status)

let mysql_stmt_reset_start stmt =
  handle_char (fun err -> B.mysql_stmt_reset_start err stmt)

let mysql_stmt_reset_cont stmt status =
  handle_char (fun err -> B.mysql_stmt_reset_cont err stmt status)

let mysql_stmt_execute_start stmt =
  handle_int (fun err -> B.mysql_stmt_execute_start err stmt)

let mysql_stmt_execute_cont stmt status =
  handle_int (fun err -> B.mysql_stmt_execute_cont err stmt status)

let mysql_stmt_fetch_start stmt =
  handle_int (fun err -> B.mysql_stmt_fetch_start err stmt)

let mysql_stmt_fetch_cont stmt status =
  handle_int (fun err -> B.mysql_stmt_fetch_cont err stmt status)

let mysql_stmt_store_result_start stmt =
  handle_int (fun err -> B.mysql_stmt_store_result_start err stmt)

let mysql_stmt_store_result_cont stmt status =
  handle_int (fun err -> B.mysql_stmt_store_result_cont err stmt status)

let mysql_stmt_close_start stmt =
  handle_char (fun err -> B.mysql_stmt_close_start err stmt)

let mysql_stmt_close_cont stmt status =
  handle_char (fun err -> B.mysql_stmt_close_cont err stmt status)

let mysql_stmt_free_result_start stmt =
  handle_char (fun err -> B.mysql_stmt_free_result_start err stmt)

let mysql_stmt_free_result_cont stmt status =
  handle_char (fun err -> B.mysql_stmt_free_result_cont err stmt status)

let mysql_commit_start mysql =
  handle_char (fun err -> B.mysql_commit_start err mysql)

let mysql_commit_cont mysql status =
  handle_char (fun err -> B.mysql_commit_cont err mysql status)

let mysql_rollback_start mysql =
  handle_char (fun err -> B.mysql_rollback_start err mysql)

let mysql_rollback_cont mysql status =
  handle_char (fun err -> B.mysql_rollback_cont err mysql status)

let mysql_autocommit_start mysql auto =
  let auto = if auto then '\001' else '\000' in
  handle_char (fun err -> B.mysql_autocommit_start err mysql auto)

let mysql_autocommit_cont mysql status =
  handle_char (fun err -> B.mysql_autocommit_cont err mysql status)

let mysql_stmt_next_result_start stmt =
  handle_int (fun err -> B.mysql_stmt_next_result_start err stmt)

let mysql_stmt_next_result_cont stmt status =
  handle_int (fun err -> B.mysql_stmt_next_result_cont err stmt status)

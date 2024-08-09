open Printf

module Make (W : Mariadb.Nonblocking.Wait) = struct
  module M = Mariadb.Nonblocking.Make (W)
  open W.IO

  let (>|=) m f = m >>= fun x -> return (f x)

  let env var def = try Sys.getenv var with Not_found -> def

  let or_die where = function
    | Ok r -> return r
    | Error (i, e) -> eprintf "%s: (%d) %s\n%!" where i e; exit 2

  let connect () =
    M.connect
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql") ()

  let rec repeat n f =
    if n = 0 then return () else f () >>= fun () -> repeat (n - 1) f

  let string_of_param_type = function
    | `Int -> "integer"
    | `Float -> "double"
    | `String | `Bytes -> "char"
    | `Time -> "datetime"

  let random_string () =
    let n = Random.int (1 lsl Random.int 8) in
    String.init n (fun _ -> "ACGT".[Random.int 4])

  let random_param_type _ =
    match Random.int 5 with
    | 0 -> `Int
    | 1 -> `Float
    | 2 -> `String
    | 3 -> `Bytes
    | 4 -> `Time
    | _ -> assert false

  let random_param param_type =
    if Random.int 6 = 0 then `Null else
    match param_type with
    | `Int -> `Int (Random.bits ())
    | `Float -> `Float (ldexp (Random.float 2.0 -. 1.0) (Random.int 16))
    | `String -> `String (random_string ())
    | `Bytes -> `Bytes (Bytes.of_string (random_string ()))
    | `Time -> `Time (M.Time.utc_timestamp (Random.float 1577833200.0))

  let make_nary_select_stmt dbh param_types =
    let buf = Buffer.create 64 in
    Buffer.add_string buf "SELECT ";
    for i = 0 to Array.length param_types - 1 do
      if i > 0 then Buffer.add_string buf ", ";
      bprintf buf "CAST(? AS %s)" (string_of_param_type param_types.(i))
      (* CAST is only used as a type annotation to prevent the parameters from
       * being cast. *)
    done;
    M.prepare dbh (Buffer.contents buf) >>= or_die "prepare"

  let string_of_timestamp t =
    let y, mon, day = M.Time.(year t, month t, day t) in
    let h, m, s, us = M.Time.(hour t, minute t, second t, microsecond t) in
    sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06d" y mon day h m s us

  let string_of_value = function
    | `Null -> "NULL"
    | `Int i -> sprintf "(%d : int)" i
    | `Float x -> sprintf "(%.8g : float)" x
    | `String s -> sprintf "(%S : string)" s
    | `Bytes s -> sprintf "(%S : bytes)" (Bytes.to_string s)
    | `Time t -> string_of_timestamp t

  let equal_float x x' =
    abs_float (x -. x') /. (abs_float (x +. x') +. epsilon_float) < 1e-6

  let equal_time t t' =
    let open M.Time in let open Stdlib in
    (* Treat `Datetime and `Timestamp as equal. *)
    year t = year t' && month t = month t' && day t = day t' &&
    hour t = hour t' && minute t = minute t' && second t = second t'

  let equal_field v v' =
    match v, v' with
    | `Null, `Null -> true
    | `Null, _ | _, `Null -> false
    | `Int i, `Int i' -> i = i'
    | `Int i, `Float x | `Float x, `Int i -> float_of_int i = x
    | `Int _, _ | _, `Int _ -> false
    | `Float x, `Float x' -> equal_float x x'
    | `Float _, _ | _, `Float _ -> false
    | `String s, `String s' -> s = s'
    | `String s, `Bytes s' | `Bytes s', `String s -> s = Bytes.to_string s'
    | `String _, _ | _, `String _ -> false
    | `Bytes s, `Bytes s' -> s = s'
    | `Bytes _, _ | _, `Bytes _ -> false
    | `Time t, `Time t' -> equal_time t t'

  let assert_field_equal v v' =
    if not (equal_field v v') then begin
      eprintf "Parameter %s came back as %s.\n%!"
              (string_of_value v) (string_of_value v');
      exit 2
    end

  (* Make sure the conversion between timestamps and strings are consistent
   * between MariaDB and OCaml. By sending timestamps to be compared as binary
   * and as string, this also verifies the MYSQL_TIME encoding. *)
  let test_datetime_and_string_conv dbh =
    let t = M.Time.utc_timestamp (Random.float 1577833200.0) in
    let s = string_of_timestamp t in
    M.prepare dbh "SELECT CAST(? AS DATETIME), DATE_FORMAT(?, '%Y-%m-%dT%T.%f')"
      >>= or_die "prepare" >>= fun stmt ->
    let params = [|`String s; `Time t|] in
    M.Stmt.execute stmt params >>= or_die "Stmt.execute" >>= fun res ->
    assert (M.Res.num_rows res = 1);
    M.Res.fetch (module M.Row.Array) res >>= or_die "Res.fetch" >|=
    (function
     | Some [|t'; s'|] ->
        assert (equal_time t M.Field.(time t'));
        assert (s = M.Field.(string s'))
     | _ -> assert false)

  let test () =
    let stmt_cache = Hashtbl.create 7 in
    connect () >>= or_die "connect" >>= fun dbh ->
    test_datetime_and_string_conv dbh >>= fun () ->
    repeat 100 begin fun () ->
      let n = Random.int (1 lsl Random.int 8) + 1 in
      let param_types = Array.init n random_param_type in
      let params = Array.map random_param param_types in
      begin
        try
          return (Hashtbl.find stmt_cache param_types)
        with Not_found ->
          make_nary_select_stmt dbh param_types
      end >>= fun stmt ->
      M.Stmt.execute stmt params >>= or_die "Stmt.execute" >>= fun res ->
      assert (M.Res.num_rows res = 1);
      M.Res.fetch (module M.Row.Array) res >>= or_die "Res.fetch" >>=
      (function
       | None -> assert false
       | Some row ->
          assert (Array.length row = Array.length params);
          for i = 0 to n - 1 do
            assert_field_equal params.(i) (M.Field.value row.(i))
          done;
          return ()) >>= fun () ->
      if Random.bool () then
        M.Stmt.close stmt >>= or_die "Stmt.close" >|= fun () ->
        Hashtbl.remove stmt_cache param_types
      else
        M.Stmt.reset stmt >>= or_die "Stmt.reset" >|= fun () ->
        Hashtbl.replace stmt_cache param_types stmt
    end >>= fun () ->
    Hashtbl.fold
      (fun _ stmt prologue ->
        prologue >>= fun () ->
        M.Stmt.close stmt >>= or_die "Stmt.close")
      stmt_cache (return ()) >>= fun () ->
    M.close dbh

  let main () = repeat 500 test
end

open Printf

module M = Mariadb.Blocking

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die where = function
  | Ok r -> r
  | Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" where i e

let print_row row =
  printf "---\n%!";
  M.Row.StringMap.iter
    (fun name field ->
      printf "%20s " name;
      match M.Field.value field with
      | `Int i -> printf "%d\n%!" i
      | `Float x -> printf "%f\n%!" x
      | `String s -> printf "%s\n%!" s
      | `Bytes b -> printf "%s\n%!" (Bytes.to_string b)
      | `Time t ->
          printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
            (M.Time.year t)
            (M.Time.month t)
            (M.Time.day t)
            (M.Time.hour t)
            (M.Time.minute t)
            (M.Time.second t)
      | `Null -> printf "NULL\n%!")
    row

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let stream res =
  let module F = struct exception E of M.error end in
  let next _ =
    match M.Res.fetch (module M.Row.Map) res with
    | Ok (Some _ as row) -> row
    | Ok None -> None
    | Error e -> raise (F.E e) in
  try Ok (Stream.from next)
  with F.E e -> Error e

let test_sqlstate mariadb =
  assert (M.sqlstate mariadb = "00000");
  (match M.prepare mariadb "SELECT * FROM inexistent_table" with
   | Error _ -> assert (M.sqlstate mariadb <> "00000") (* actually "42S02" *)
   | Ok _ -> assert false);
  begin
    let stmt =
      M.prepare mariadb
                "CREATE TEMPORARY TABLE test_sqlstate (i integer PRIMARY KEY)"
        |> or_die "prepare CREATE TABLE test_sqlstate"
    in
    let _ =
      M.Stmt.execute stmt [||]
        |> or_die "exec CREATE TABLE test_sqlstate"
    in
    M.Stmt.close stmt |> or_die "stmt close CREATE TABLE test_sqlstate"
  end;
  for i = 0 to 1 do
    let stmt =
      M.prepare mariadb "INSERT INTO test_sqlstate VALUES (?)"
        |> or_die "prepare in test_sqlstate"
    in
    (match M.Stmt.execute stmt [|`Int 1|] with
     | Error (_, msg) ->
        assert (i = 1);
        assert (M.Stmt.sqlstate stmt <> "00000") (* actually "23000" *)
     | Ok _ -> assert (i = 0));

    M.Stmt.close stmt |> or_die "stmt close in test_sqlstate"
  done

let main () =
  let mariadb = connect () |> or_die "connect" in
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM mysql.user WHERE User LIKE ?" in
  let stmt = M.prepare mariadb query |> or_die "prepare" in
  let res = M.Stmt.execute stmt [| `String "r%" |] |> or_die "exec" in
  assert (M.Res.affected_rows res = M.Res.num_rows res);
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  let s = stream res |> or_die "stream" in
  Stream.iter print_row s;
  M.Stmt.close stmt |> or_die "stmt close";
  test_sqlstate mariadb;
  M.close mariadb;
  M.library_end ();
  printf "done\n%!"

let () = main ()

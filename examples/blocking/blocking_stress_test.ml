open Printf

module M = Mariadb.Blocking

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die where = function
  | Ok r -> r
  | Error (i, e) -> ksprintf failwith "%s: (%d) %s" where i e

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let test () =
  let dbh = connect () |> or_die "connect" in
  (* without CAST result is typed as NULL for some reason *)
  let mk_stmt () =  M.prepare dbh "SELECT CAST(? AS BINARY)" |> or_die "prepare" in
  let stmt = ref (mk_stmt ()) in
  for _ = 1 to 100 do
    let n = Random.int (1 lsl Random.int 8) in
    let s = String.init n (fun _ -> "ACGT".[Random.int 4]) in
    let res = M.Stmt.execute !stmt [|`String s|] |> or_die "Stmt.execute" in
    assert (M.Res.num_rows res = 1);
    (match M.Res.fetch (module M.Row.Array) res |> or_die "Res.fetch" with
     | None -> assert false
     | Some row ->
         let s' = M.Field.string row.(0) in
         if s <> s' then printf "@@@ <%s> <%s>\n%!" s s';
         assert (s = s'));
    if Random.bool () then begin
      M.Stmt.close !stmt |> or_die "Stmt.close";
      stmt := mk_stmt ()
    end else
      M.Stmt.reset !stmt |> or_die "Stmt.reset"
  done;
  M.Stmt.close !stmt |> or_die "Stmt.close";
  M.close dbh

let () = for _ = 1 to 500 do test () done

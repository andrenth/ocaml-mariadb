module IO = struct
  type 'a future = 'a
  let (>>=) x f = f x
  let return x = x
end

open IO

module S = Mariadb.Nonblocking.Status
module M = Mariadb.Nonblocking.Make(struct
  module IO = IO

  let wait mariadb status =
    let fd = Mariadb.Nonblocking.fd mariadb in
    let rfd = if S.read status then [fd] else [] in
    let wfd = if S.write status then [fd] else [] in
    let efd = if S.except status then [fd] else [] in
    let timeout =
      if S.timeout status
      then float @@ Mariadb.Nonblocking.timeout mariadb
      else -1.0 in
    try
      let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
      return @@
        S.create
          ~read:(rfd <> [])
          ~write:(wfd <> [])
          ~except:(efd <> [])
          ()
    with Unix.Unix_error (_, _, _) ->
      return @@ S.create ~timeout: true ()
end)

open Printf

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die ?(info = "error") () = function
  | Ok r -> return r
  | Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" info i e

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

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
    row;
  return ()

let rec each_row res f =
  match M.Res.fetch (module M.Row.Map) res with
  | Ok (Some row) -> f row; each_row res f
  | Ok None -> return ()
  | Error (_, s) -> failwith @@ "fetch: " ^ s

let main () =
  let mariadb = connect () |> or_die ~info:"connect" () in
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM mysql.user WHERE User LIKE ?" in
  let stmt = M.prepare mariadb query |> or_die ~info:"prepare" () in
  let res = M.Stmt.execute stmt [| `String "r%" |] |> or_die () in
  assert (M.Res.affected_rows res = M.Res.num_rows res);
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  each_row res print_row;
  M.Stmt.close stmt |> or_die ();
  M.close mariadb;
  M.library_end ()

let () = main ()

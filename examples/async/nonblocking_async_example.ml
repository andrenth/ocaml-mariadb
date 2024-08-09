module Caml_bytes = Bytes
open Core
open Async

module S = Mariadb.Nonblocking.Status
module M = Mariadb.Nonblocking.Make(struct
  module IO = struct
    type 'a future = 'a Deferred.t
    let (>>=) = (>>=)
    let return = Deferred.return
  end

  let is_ready = function
    | `Ready -> true
    | `Bad_fd | `Closed -> false

  let ready (rt, wt, tt) =
    let r = ref false in
    let w = ref false in
    let t = ref false in
    let rc = Deferred.choice rt (fun x -> r := is_ready x) in
    let wc = Deferred.choice wt (fun x -> w := is_ready x) in
    let tc = Deferred.choice tt (fun _ -> t := true) in
    Deferred.enabled [rc; wc; tc] >>= fun f ->
    ignore (f ());
    Deferred.return (!r, !w, !t)

  let wait mariadb status =
    let fd =
      Fd.create
        (Fd.Kind.Socket `Active)
        (Mariadb.Nonblocking.fd mariadb)
        (Info.of_string "<mariadb fd>") in
    assert (S.read status || S.write status || S.timeout status);
    let idle = Deferred.never () in
    let rt =
      if S.read status then Fd.ready_to fd `Read
      else idle in
    let wt =
      if S.write status then begin Fd.ready_to fd `Write
      end
      else idle in
    let tt =
      let tmout = float (Mariadb.Nonblocking.timeout mariadb) in
      if S.timeout status then Clock.after (Time.Span.of_sec tmout)
      else idle in
    ready (rt, wt, tt) >>= fun (read, write, timeout) ->
    Fd.close ~file_descriptor_handling:Fd.Do_not_close_file_descriptor fd
    >>= fun () ->
    Deferred.return @@ S.create ~read ~write ~timeout ()
end)

let env var def =
  match Sys.getenv var with
  | Some v -> v
  | None -> def

let or_die where = function
  | Stdlib.Ok r -> return r
  | Stdlib.Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" where i e

let print_row row =
  printf "---\n%!";
  M.Row.StringMap.fold
    (fun name field _ ->
      printf "%20s " name;
      match M.Field.value field with
      | `Int i -> printf "%d\n%!" i
      | `Float x -> printf "%f\n%!" x
      | `String s -> printf "%s\n%!" s
      | `Bytes b -> printf "%s\n%!" (Caml_bytes.to_string b)
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
    ();
  return ()

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let stream res =
  let build () =
    M.Res.fetch (module M.Row.Map) res
    >>| function
      | Ok (Some row) -> Some (row, ())
      | Ok None | Error _ -> None in
  return (Pipe.unfold ~init:() ~f:build)

let print_rows p =
  Pipe.iter p ~f:print_row

let _main : unit Deferred.t =
  connect () >>= or_die "connect" >>= fun mariadb ->
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM mysql.user WHERE User LIKE ?" in
  M.prepare mariadb query >>= or_die "prepare" >>= fun stmt ->
  M.Stmt.execute stmt [| `String "r%" |] >>= or_die "exec" >>= fun res ->
  assert (M.Res.affected_rows res = M.Res.num_rows res);
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  stream res >>= fun p ->
  print_rows p >>= fun () ->
  M.Stmt.close stmt >>= or_die "stmt close" >>= fun () ->
  M.close mariadb >>= fun () ->
  M.library_end ();
  Shutdown.exit 0

let () = never_returns (Scheduler.go ())

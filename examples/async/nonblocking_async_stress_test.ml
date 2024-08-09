module Caml_bytes = Bytes
open Core
open Async

module S = Mariadb.Nonblocking.Status

module Test = Nonblocking_stress_test.Make (struct

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
    let rt = if S.read status then Fd.ready_to fd `Read else idle in
    let wt = if S.write status then Fd.ready_to fd `Write else idle in
    let tt =
      let tmout = float (Mariadb.Nonblocking.timeout mariadb) in
      if S.timeout status then Clock.after (Time.Span.of_sec tmout)
      else idle in
    ready (rt, wt, tt) >>= fun (read, write, timeout) ->
    Fd.close ~file_descriptor_handling:Fd.Do_not_close_file_descriptor fd
    >>= fun () ->
    Deferred.return @@ S.create ~read ~write ~timeout ()

end)

let _main : unit Deferred.t = Test.main () >>= fun () -> Shutdown.exit 0

let () = never_returns (Scheduler.go ())

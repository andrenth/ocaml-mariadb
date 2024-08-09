module S = Mariadb.Nonblocking.Status

module Test = Nonblocking_stress_test.Make (struct
  module IO = struct
    type 'a future = 'a
    let (>>=) x f = f x
    let return x = x
  end

  let return = IO.return

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

let () = Test.main ()

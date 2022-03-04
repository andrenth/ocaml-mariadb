module C = Configurator.V1

let detect_src = {|
#include <stddef.h>
#ifdef MARIADB_CLIENT
#include <mysql/mysql.h>
#else
#include <mariadb/mysql.h>
#endif

int
main(void)
{
    MYSQL *m = mysql_init(NULL);
    mysql_close(m);
    return 0;
}
|}

module Variant = struct
  type t = {
    c_flags: string list;
    link_flags: string list;
    include_base: string;
  }

  let try_compile c {c_flags; link_flags; _} =
    C.c_test c detect_src ~c_flags ~link_flags
end

let variants = Variant.[
  {c_flags = []; link_flags = ["-lmariadb"];
   include_base = "mariadb"};
  {c_flags = ["-DMARIADB_CLIENT"]; link_flags = ["-lmariadbclient"];
   include_base = "mysql"};
  {c_flags = ["-DMARIADB_CLIENT"]; link_flags = ["-lmysqlclient"];
   include_base = "mysql"};
]

let () = C.main ~name:"mariadb" @@ fun c ->

  let variant =
    try List.find (Variant.try_compile c) variants
    with Not_found -> C.die "Cannot find MariaDB client library."
  in
  C.Flags.write_sexp "mariadb_link_flags.sexp" variant.Variant.link_flags;
  C.Flags.write_lines "mariadb_preamble.h" [
    Printf.sprintf "#include <%s/mysql.h>" variant.Variant.include_base;
  ]

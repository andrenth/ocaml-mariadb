open Ctypes

module T = Ffi_bindings.Types(Ffi_generated_types)

type t =
  { n : int
  ; bind : T.Bind.t ptr
  ; length : Unsigned.ulong ptr
  ; is_null : char ptr
  ; is_unsigned : char
  ; error : char ptr
  }

type buffer_type =
  [ `Null
  | `Tiny
  | `Year
  | `Short
  | `Int24
  | `Long
  | `Float
  | `Long_long
  | `Double
  | `Decimal
  | `New_decimal
  | `String
  | `Var_string
  | `Tiny_blob
  | `Blob
  | `Medium_blob
  | `Long_blob
  | `Bit
  | `Time
  | `Date
  | `Datetime
  | `Timestamp
  ]

let buffer_type_of_int i =
  let open T.Type in
  if i = null              then `Null
  else if i = tiny         then `Tiny
  else if i = year         then `Year
  else if i = short        then `Short
  else if i = int24        then `Int24
  else if i = long         then `Long
  else if i = float        then `Float
  else if i = long_long    then `Long_long
  else if i = double       then `Double
  else if i = decimal      then `Decimal
  else if i = new_decimal  then `New_decimal
  else if i = string       then `String
  else if i = var_string   then `Var_string
  else if i = tiny_blob    then `Tiny_blob
  else if i = blob         then `Blob
  else if i = medium_blob  then `Medium_blob
  else if i = long_blob    then `Long_blob
  else if i = bit          then `Bit
  else if i = time         then `Time
  else if i = date         then `Date
  else if i = datetime     then `Datetime
  else if i = timestamp    then `Timestamp
  else invalid_arg @@ "unknown buffer type " ^ (string_of_int i)

let t = '\001'
let f = '\000'

let alloc count =
  { n = count
  ; bind = allocate_n T.Bind.t ~count
  ; length = allocate_n ulong ~count
  ; is_null = allocate_n char ~count
  ; is_unsigned = f
  ; error = allocate_n char ~count
  }

let bind b ~buffer ~size ~mysql_type ~unsigned ~at =
  assert (at >= 0 && at < b.n);
  let size = Unsigned.ULong.of_int size in
  let bp = b.bind +@ at in
  let lp = b.length +@ at in
  lp <-@ size;
  setf (!@bp) T.Bind.length lp;
  setf (!@bp) T.Bind.is_unsigned unsigned;
  setf (!@bp) T.Bind.buffer_type mysql_type;
  setf (!@bp) T.Bind.buffer_length size;
  setf (!@bp) T.Bind.buffer buffer

let tiny ?(unsigned = false) b param ~at =
  let p = allocate char (char_of_int param) in
  bind b
    ~buffer:(coerce (ptr char) (ptr void) p)
    ~size:(sizeof int)
    ~mysql_type:T.Type.tiny
    ~unsigned:(if unsigned then t else f)
    ~at

let short ?(unsigned = false) b param ~at =
  let p = allocate short param in
  bind b
    ~buffer:(coerce (ptr short) (ptr void) p)
    ~size:(sizeof int)
    ~mysql_type:T.Type.short
    ~unsigned:(if unsigned then t else f)
    ~at

let int ?(unsigned = false) b param ~at =
  let p = allocate int param in
  bind b
    ~buffer:(coerce (ptr int) (ptr void) p)
    ~size:(sizeof int)
    ~mysql_type:T.Type.long_long
    ~unsigned:(if unsigned then t else f)
    ~at

let float b param ~at =
  let p = allocate float param in
  bind b
    ~buffer:(coerce (ptr float) (ptr void) p)
    ~size:(sizeof float)
    ~mysql_type:T.Type.float
    ~unsigned:f
    ~at

let double b param ~at =
  let p = allocate double param in
  bind b
    ~buffer:(coerce (ptr double) (ptr void) p)
    ~size:(sizeof double)
    ~mysql_type:T.Type.double
    ~unsigned:f
    ~at

let string b param ~at =
  let len = String.length param in
  let p = allocate_n char ~count:len in
  String.iteri (fun i c -> (p +@ i) <-@ c) param;
  bind b
    ~buffer:(coerce (ptr char) (ptr void) p)
    ~size:len
    ~mysql_type:T.Type.string
    ~unsigned:f
    ~at

let blob b param ~at =
  let len = Bytes.length param in
  let p = allocate_n char ~count:len in
  String.iteri (fun i c -> (p +@ i) <-@ c) param;
  bind b
    ~buffer:(coerce (ptr char) (ptr void) p)
    ~size:len
    ~mysql_type:T.Type.blob
    ~unsigned:f
    ~at
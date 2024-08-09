open Ctypes

module T = Ffi_generated.Types

type value =
  [ `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Bytes of bytes
  | `Time of Time.t
  ]

type t =
  { result : Bind.t
  ; pointer : T.Field.t ptr
  ; at : int
  }

let create result pointer at =
  { result; pointer; at }

let name field =
  getf (!@(field.pointer)) T.Field.name

let null_value field =
  !@(field.result.Bind.is_null +@ field.at) = '\001'

let can_be_null field =
  let flags = getf (!@(field.pointer)) T.Field.flags in
  Unsigned.UInt.logand flags T.Field.Flags.not_null = Unsigned.UInt.zero

let is_unsigned field =
  let bp = field.result.Bind.bind +@ field.at in
  getf (!@bp) T.Bind.is_unsigned = '\001'

let buffer field =
  let bp = field.result.Bind.bind +@ field.at in
  getf (!@bp) T.Bind.buffer

let cast_to typ field =
  !@(coerce (ptr void) (ptr typ) (buffer field))

let to_bytes field =
  let buf = buffer field in
  let r = field.result in
  let lp = r.Bind.length +@ field.at in
  let len = Unsigned.ULong.to_int !@lp in
  let p = coerce (ptr void) (ptr char) buf in
  Bytes.init len (fun i -> !@(p +@ i))

let to_time field kind =
  let buf = buffer field in
  let tp = coerce (ptr void) (ptr T.Time.t) buf in
  let member f = Unsigned.UInt.to_int @@ getf (!@tp) f in
  let member_long f = Unsigned.ULong.to_int @@ getf (!@tp) f in
  { Time.
    year   = member T.Time.year
  ; month  = member T.Time.month
  ; day    = member T.Time.day
  ; hour   = member T.Time.hour
  ; minute = member T.Time.minute
  ; second = member T.Time.second
  ; microsecond = member_long T.Time.second_part
  ; kind
  }

type to_string = [`Decimal | `New_decimal | `String | `Var_string | `Bit]
type to_blob   = [`Tiny_blob | `Blob | `Medium_blob | `Long_blob]
type to_time   = [`Time | `Date | `Datetime | `Timestamp]

let convert field typ unsigned =
  let open Signed in
  let open Unsigned in
  match typ, unsigned with
  | `Null,                _ -> `Null
  | `Year,                _
  | `Tiny,             true -> `Int (int_of_char (cast_to char field))
  | `Tiny,            false -> `Int (cast_to schar field)
  | `Short,            true -> `Int (cast_to int field)
  | `Short,           false -> `Int (UInt.to_int (cast_to uint field))
  | (`Int24 | `Long),  true -> `Int (UInt32.to_int (cast_to uint32_t field))
  | (`Int24 | `Long), false -> `Int (Int32.to_int (cast_to int32_t field))
  | `Long_long,        true -> `Int (UInt64.to_int (cast_to uint64_t field))
  | `Long_long,       false -> `Int (Int64.to_int (cast_to int64_t field))
  | `Float,               _ -> `Float (cast_to float field)
  | `Double,              _ -> `Float (cast_to double field)
  | #to_string,           _ -> `String (Bytes.to_string (to_bytes field))
  | #to_blob,             _ -> `Bytes (to_bytes field)
  | #to_time as t,        _ -> `Time (to_time field t)

let value field =
  let bp = field.result.Bind.bind +@ field.at in
  if null_value field then `Null
  else
    let typ = Bind.buffer_type_of_int @@ getf (!@bp) T.Bind.buffer_type in
    convert field typ (is_unsigned field)

let err field ~info =
  failwith @@ "field '" ^ name field ^ "' is not " ^ info

let int field =
  match value field with
  | `Int i -> i
  | _ -> err field ~info:"an integer"

let float field =
  match value field with
  | `Float x -> x
  | _ -> err field ~info:"a float"

let string field =
  match value field with
  | `String s -> s
  | _ -> err field ~info:"a string"

let bytes field =
  match value field with
  | `Bytes b -> b
  | _ -> err field ~info:"a byte string"

let time field =
  match value field with
  | `Time t -> t
  | _ -> err field ~info:"a time value"

let int_opt field =
  match value field with
  | `Int i -> Some i
  | `Null -> None
  | _ -> err field ~info:"a nullable integer"

let float_opt field =
  match value field with
  | `Float x -> Some x
  | `Null -> None
  | _ -> err field ~info:"a nullable float"

let string_opt field =
  match value field with
  | `String s -> Some s
  | `Null -> None
  | _ -> err field ~info:"a nullable string"

let bytes_opt field =
  match value field with
  | `Bytes b -> Some b
  | `Null -> None
  | _ -> err field ~info:"a nullable byte string"

let time_opt field =
  match value field with
  | `Time t -> Some t
  | `Null -> None
  | _ -> err field ~info:"a nullable time value"

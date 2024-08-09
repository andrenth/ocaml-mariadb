module T = Ffi_generated.Types

type t = int

let create ?(read = false) ?(write = false) ?(except = false)
           ?(timeout = false) () =
  let w = ref 0 in
  if read    then w := !w lor T.Wait_status.read;
  if write   then w := !w lor T.Wait_status.write;
  if except  then w := !w lor T.Wait_status.except;
  if timeout then w := !w lor T.Wait_status.timeout;
  !w

let of_int w = w
let to_int w = w

let read w    = w land T.Wait_status.read > 0
let write w   = w land T.Wait_status.write > 0
let except w  = w land T.Wait_status.except > 0
let timeout w = w land T.Wait_status.timeout > 0

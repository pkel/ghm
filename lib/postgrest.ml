open Core_kernel

module type REQUEST = sig
  type t

  val delete : string -> t
  val get : string -> t
  val patch : string -> t
  val post : string -> t
  val put : string -> t
  val header : key:string -> value:string -> t -> t
  val body : string -> t -> t
end

module Query = struct
  type 'a column = string

  let column name = name
  let string = column
  let int = column
  let bool = column
  let date = column

  type select = string

  let select column = column

  type order = string

  let order order ?null column =
    sprintf
      "%s.%s%s"
      column
      order
      (match null with
      | None -> ""
      | Some `First -> ".nullsfirst"
      | Some `Last -> ".nullslast")
  ;;

  let desc = order "desc"
  let asc = order "asc"

  (* TODO: is this a nice encoding? *)
  type constr =
    { outer : unit -> string
    ; inner : unit -> string
    }

  let not_ { inner; _ } =
    { outer = (fun () -> sprintf "not=(%s)" (inner ()))
    ; inner = (fun () -> sprintf "not(%s)" (inner ()))
    }
  ;;

  let and_ c1 c2 =
    { outer = (fun () -> sprintf "and=(%s,%s)" (c1.inner ()) (c2.inner ()))
    ; inner = (fun () -> sprintf "and(%s,%s)" (c1.inner ()) (c2.inner ()))
    }
  ;;

  let or_ c1 c2 =
    { outer = (fun () -> sprintf "or=(%s,%s)" (c1.inner ()) (c2.inner ()))
    ; inner = (fun () -> sprintf "or(%s,%s)" (c1.inner ()) (c2.inner ()))
    }
  ;;
end

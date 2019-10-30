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

module Column = struct
  type 'a t = string

  let column name = name
  let string = column
  let int = column
  let bool = column
  let date = column
end

module Query = struct
  type t = string

  let to_string x = x

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

  let opn name left right args =
    let args = List.map ~f:(fun x -> x.inner ()) args |> String.concat ~sep:"," in
    { outer = (fun () -> sprintf "%s=%s%s%s" name left args right)
    ; inner = (fun () -> sprintf "%s%s%s%s" name left args right)
    }
  ;;

  let op1 name left right arg = opn name left right [ arg ]
  let op2 name left right a b = opn name left right [ a; b ]
  let ( ! ) = op1 "not" "(" ")"
  let ( && ) = op2 "and" "(" ")"
  let ( || ) = op2 "or" "(" ")"

  let create ?(select = []) ?filter ?(order = []) ?limit ?offset resource =
    [ (match select with
      | [] -> None
      | _ -> Some (sprintf "select=%s" (String.concat ~sep:"," select)))
    ; Option.map ~f:(fun { outer; _ } -> outer ()) filter
    ; (match order with
      | [] -> None
      | _ -> Some (sprintf "order=%s" (String.concat ~sep:"," order)))
    ; Option.map ~f:(fun i -> sprintf "limit=%i" i) limit
    ; Option.map ~f:(fun i -> sprintf "offset=%i" i) offset
    ]
    |> List.filter_opt
    |> String.concat ~sep:"&"
    |> function
    | "" -> resource
    | params -> sprintf "%s?%s" resource params
  ;;

  let op to_string op col x =
    { inner = (fun () -> sprintf "%s.%s.%s" col op (to_string x))
    ; outer = (fun () -> sprintf "%s=%s.%s" col op (to_string x))
    }
  ;;

  module type TYP = sig
    type t

    val f : t -> string
  end

  module type EQUAL = sig
    type t

    val ( = ) : t Column.t -> t -> constr
    val ( <> ) : t Column.t -> t -> constr
  end

  module type COMPARE = sig
    type t

    val ( > ) : t Column.t -> t -> constr
    val ( >= ) : t Column.t -> t -> constr
    val ( < ) : t Column.t -> t -> constr
    val ( <= ) : t Column.t -> t -> constr
  end

  module type TEXT = sig
    type t

    val like : t Column.t -> t -> constr
    val ilike : t Column.t -> t -> constr
  end

  module Equal (T : TYP) : EQUAL with type t := T.t = struct
    let op = op T.f
    let ( = ) = op "eq"
    let ( <> ) = op "neq"
  end

  module Compare (T : TYP) : COMPARE with type t := T.t = struct
    let op = op T.f
    let ( < ) = op "lt"
    let ( > ) = op "gt"
    let ( <= ) = op "lte"
    let ( >= ) = op "gte"
  end

  module Text (T : TYP) : TEXT with type t := T.t = struct
    let op = op T.f
    let like = op "like"
    let ilike = op "ilike"
  end

  module Bool = struct
    type t = bool

    module T : TYP with type t = t = struct
      type nonrec t = t

      let f x = if x then "true" else "false"
    end

    include Equal (T)
  end

  module String = struct
    type t = string

    module T : TYP with type t = t = struct
      type nonrec t = t

      let f x = x
    end

    include Equal (T)
    include Text (T)
  end

  module Int = struct
    type t = int

    module T : TYP with type t = t = struct
      type nonrec t = t

      let f = Int.to_string
    end

    include Equal (T)
    include Compare (T)
  end

  module Date = struct
    type t = Date.t

    module T : TYP with type t = t = struct
      type nonrec t = t

      let f = Date.to_string
    end

    include Equal (T)
    include Compare (T)
  end
end

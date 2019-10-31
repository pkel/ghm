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

type 'a resource = string
type ('a, 'b) column = string

module Setup = struct
  let resource s = s

  module Column = struct
    type ('a, 'b) creator = 'a resource -> string -> ('a, 'b) column

    let column _ name = name
    let string = column
    let int = column
    let bool = column
    let date = column
  end
end

module Query = struct
  type 'a t = string

  let to_string x = x

  type 'a select = string

  let select column = column

  type 'a order = string

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

  type _constr =
    { outer : string lazy_t
    ; inner : string lazy_t
    }

  type 'a constr = _constr

  let opn name left right args =
    let args = List.map ~f:(fun x -> force x.inner) args |> String.concat ~sep:"," in
    { outer = lazy (sprintf "%s=%s%s%s" name left args right)
    ; inner = lazy (sprintf "%s%s%s%s" name left args right)
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
    ; Option.map ~f:(fun x -> force x.outer) filter
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

  type ('a, 'b) op = ('a, 'b) column -> 'b -> 'a constr

  let op to_string op col x =
    { inner = lazy (sprintf "%s.%s.%s" col op (to_string x))
    ; outer = lazy (sprintf "%s=%s.%s" col op (to_string x))
    }
  ;;

  module type TYP = sig
    type t

    val f : t -> string
  end

  module type EQUAL = sig
    type t

    val ( = ) : ('a, t) op
    val ( <> ) : ('a, t) op
  end

  module type COMPARE = sig
    type t

    val ( > ) : ('a, t) op
    val ( >= ) : ('a, t) op
    val ( < ) : ('a, t) op
    val ( <= ) : ('a, t) op
  end

  module type TEXT = sig
    type t

    val like : ('a, t) op
    val ilike : ('a, t) op
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

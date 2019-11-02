open Yojson.Safe
open Core_kernel

type ('a, 'b) column = string

module Resource = struct
  type ('a, 'b, 'c) t =
    { provide : 'b -> json
    ; return : json -> 'c Or_error.t
    ; name : string
    ; select : string list
    }

  module type SPEC = sig
    val name : string
    val select : string list

    type provide
    type return

    val provide : provide -> json
    val return : json -> return Or_error.t
  end

  module Create (S : SPEC) = struct
    open S

    type provide = S.provide
    type return = S.return
    type 'a resource = ('a, provide, return) t
    type t

    let t : t resource = { provide; return; name; select }

    module Column_creator = struct
      type 'a creator = string -> (t, 'a) column

      let gen x = x
      let int = gen
      let string = gen
      let bool = gen
      let date = gen
    end
  end
end

module Query = struct
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
    { param : Request.Url.param lazy_t
    ; inner : string lazy_t
    }

  type 'a constr = _constr

  let opn name left right args =
    let args = List.map ~f:(fun x -> force x.inner) args |> String.concat ~sep:"," in
    { param =
        lazy
          (Request.Url.param ~key:name ~value:(Some (sprintf "%s%s%s" left args right)))
    ; inner = lazy (sprintf "%s%s%s%s" name left args right)
    }
  ;;

  let op1 name left right arg = opn name left right [ arg ]
  let op2 name left right a b = opn name left right [ a; b ]
  let ( ! ) = op1 "not" "(" ")"
  let ( && ) = op2 "and" "(" ")"
  let ( || ) = op2 "or" "(" ")"

  type ('a, 'b) op = ('a, 'b) column -> 'b -> 'a constr

  let op to_string op col x =
    { param =
        lazy
          (Request.Url.param ~key:col ~value:(Some (sprintf "%s.%s" op (to_string x))))
    ; inner = lazy (sprintf "%s.%s.%s" col op (to_string x))
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

module Make
    (Request : Request.REQUEST) (Conv : sig
        val to_json : Request.body -> json Or_error.t
        val of_json : json -> Request.body
    end) =
struct
  open Request
  open Resource

  let request = create

  let give_s r =
    give ~content_type:"application/json" ~f:(Fn.compose Conv.of_json r.provide)
  ;;

  let want_s r =
    let f body = Conv.to_json body |> Or_error.bind ~f:r.return in
    want ~accept:"application/json" ~f
  ;;

  let want_m r =
    let f body =
      Conv.to_json body
      |> Or_error.(bind ~f:(fun json -> Util.convert_each r.return json |> Or_error.all))
    in
    want ~accept:"application/json" ~f
  ;;

  let create r =
    request POST (Url.url r.name [])
    |> give_s r
    |> header ~key:"Prefer" ~value:"return=representation"
    |> want_s r
  ;;

  let read ?filter ?(order = []) ?limit ?offset r =
    let open Url in
    let params =
      [ Option.map ~f:(fun x -> Query.(force x.param)) filter
      ; (match order with
        | [] -> None
        | _ -> Some (param ~key:"order" ~value:(Some (String.concat ~sep:"," order))))
      ; Option.map ~f:(fun i -> param ~key:"limit" ~value:(Some (Int.to_string i))) limit
      ; Option.map
          ~f:(fun i -> param ~key:"offset" ~value:(Some (Int.to_string i)))
          offset
      ]
      |> List.filter_opt
    in
    request GET (Url.url r.name params) |> want_m r
  ;;

  let update filter r =
    request PUT (Url.url r.name Query.[ force filter.param ])
    |> give_s r
    |> header ~key:"Prefer" ~value:"return=representation"
    |> want_m r
  ;;

  let delete filter r = request PUT (Url.url r.name Query.[ force filter.param ])
end

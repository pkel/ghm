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

    type a
    type b

    val provide : a -> json
    val return : json -> b Or_error.t
  end

  module Create (S : SPEC) = struct
    open S

    type 'a resource = ('a, S.a, S.b) t
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
    { param = lazy (Request.Url.param name (sprintf "%s%s%s" left args right))
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
    { param = lazy (Request.Url.param col (sprintf "%s.%s" op (to_string x)))
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

module type REQUEST = sig
  include Request.REQUEST

  val give_json : ?content_type:string -> (unit, 'b) t -> (json, 'b) t
  val want_json : ?accept:string -> ('a, unit) t -> ('a, json) t
end

module Url = Request.Url

module Make (Request : REQUEST) = struct
  open Request
  open Resource

  let request = create

  let conv_resp_list ~f =
    conv_resp ~f:(fun json -> Util.convert_each f json |> Or_error.all)
  ;;

  let select r = Url.param "select" (String.concat ~sep:"," r.select)

  let create r =
    request POST Url.(url r.name [ select r ])
    |> give_json
    |> map ~f:r.provide
    |> want_json ~accept:"application/vnd.pgrst.object+json"
    |> header ~key:"Prefer" ~value:"return=representation"
    |> conv_resp ~f:r.return
  ;;

  let read ?filter ?(order = []) ?limit ?offset r =
    let open Url in
    let params =
      [ Some (select r)
      ; Option.map ~f:(fun x -> Query.(force x.param)) filter
      ; (match order with
        | [] -> None
        | _ -> Some (param "order" (String.concat ~sep:"," order)))
      ; Option.map ~f:(fun i -> param "limit" (Int.to_string i)) limit
      ; Option.map ~f:(fun i -> param "offset" (Int.to_string i)) offset
      ]
      |> List.filter_opt
    in
    request GET (Url.url r.name params) |> want_json |> conv_resp_list ~f:r.return
  ;;

  let update filter r =
    request PATCH (Url.url r.name Query.[ force filter.param; select r ])
    |> give_json
    |> map ~f:r.provide
    |> header ~key:"Prefer" ~value:"return=representation"
    |> want_json
    |> conv_resp_list ~f:r.return
  ;;

  let delete filter r = request DELETE (Url.url r.name Query.[ force filter.param ])
end

open Yojson.Safe
open Core_kernel

type ('a, 'b) column

module Resource : sig
  type ('a, 'b, 'c) t

  module type SPEC = sig
    val name : string
    val select : string list

    type provide
    type return

    val provide : provide -> json
    val return : json -> return Or_error.t
  end

  module Create (S : SPEC) : sig
    type provide = S.provide
    type return = S.return
    type 'a resource = ('a, provide, return) t
    type t

    val t : t resource

    module Column_creator : sig
      [@@@ocaml.deprecated "Use this module only once and as early as possible!"]

      (* TODO: does this work? Then use 4.08 alert feature *)

      type 'a creator = string -> (t, 'a) column

      val int : int creator
      val string : string creator
      val bool : bool creator
      val date : Date.t creator
    end
  end
end

module Query : sig
  type 'a order

  val desc : ?null:[ `First | `Last ] -> ('a, 'b) column -> 'a order
  val asc : ?null:[ `First | `Last ] -> ('a, 'b) column -> 'a order

  type 'a constr

  val ( ! ) : 'a constr -> 'a constr
  val ( && ) : 'a constr -> 'a constr -> 'a constr
  val ( || ) : 'a constr -> 'a constr -> 'a constr

  type ('a, 'b) op = ('a, 'b) column -> 'b -> 'a constr

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

  module Bool : sig
    type t = bool

    include EQUAL with type t := t
  end

  module String : sig
    type t = string

    include EQUAL with type t := t
    include TEXT with type t := t
  end

  module Int : sig
    type t = int

    include EQUAL with type t := t
    include COMPARE with type t := t
  end

  module Date : sig
    type t = Date.t

    include EQUAL with type t := t
    include COMPARE with type t := t
  end
end

module type REQUEST = sig
  include Request.REQUEST

  val give_json : ?content_type:string -> (unit, 'b) t -> (json, 'b) t
  val want_json : ?accept:string -> ('a, unit) t -> ('a, json) t
end

module Make (Request : REQUEST) : sig
  open Query

  val create : ('a, 'b, 'c) Resource.t -> ('b, 'c) Request.t

  val read
    :  ?filter:'a constr
    -> ?order:'a order list
    -> ?limit:int
    -> ?offset:int
    -> ('a, 'b, 'c) Resource.t
    -> (unit, 'c list) Request.t

  val update : 'a constr -> ('a, 'b, 'c) Resource.t -> ('b, 'c list) Request.t
  val delete : 'a constr -> ('a, 'b, 'c) Resource.t -> (unit, unit) Request.t
end

open Core_kernel

type json = Yojson.Safe.t
type ('a, 'b) column
type ('a, 'b) key

module Resource : sig
  type ('a, 'b, 'c) t

  module type SPEC = sig
    val name : string
    val select : string list

    type a
    type b

    val provide : a -> json
    val return : json -> b Or_error.t
  end

  module Create (S : SPEC) : sig
    type 'a resource = ('a, S.a, S.b) t
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
      val key : (t, 'a) column -> (t, 'a) key
    end
  end
end

module Query : sig
  type 'a order

  val desc : ?null:[ `First | `Last ] -> ('a, 'b) column -> 'a order
  val asc : ?null:[ `First | `Last ] -> ('a, 'b) column -> 'a order

  type 'a constr
  type 'a unique

  val ( ! ) : 'a constr -> 'a constr
  val ( && ) : 'a constr -> 'a constr -> 'a constr
  val ( || ) : 'a constr -> 'a constr -> 'a constr

  type ('a, 'b) op = ('a, 'b) column -> 'b -> 'a constr

  module type EQUAL = sig
    type t

    val ( = ) : ('a, t) op
    val ( <> ) : ('a, t) op
    val ( == ) : ('a, t) key -> t -> 'a unique
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

module Request = Request

module type REQUEST = sig
  include Request.REQUEST

  val give_json : ?content_type:string -> (unit, 'b) t -> (json, 'b) t
  val want_json : ?accept:string -> ('a, unit) t -> ('a, json) t
end

module Make (Request : REQUEST) : sig
  open Query

  val create : ('a, 'b, 'c) Resource.t -> ('b, 'c) Request.t
  val create_m : ('a, 'b, 'c) Resource.t -> ('b list, unit) Request.t

  val read
    :  ?filter:'a constr
    -> ?order:'a order list
    -> ?limit:int
    -> ?offset:int
    -> ('a, 'b, 'c) Resource.t
    -> (unit, 'c list) Request.t

  val read' : 'a unique -> ('a, 'b, 'c) Resource.t -> (unit, 'c) Request.t
  val update : 'a constr -> ('a, 'b, 'c) Resource.t -> ('b, 'c list) Request.t
  val update' : 'a unique -> ('a, 'b, 'c) Resource.t -> ('b, 'c) Request.t
  val delete : 'a constr -> ('a, 'b, 'c) Resource.t -> (unit, unit) Request.t
end

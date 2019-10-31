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

type 'a resource
type ('a, 'b) column

module Setup : sig
  val resource : string -> 'a resource

  module Column : sig
    type ('a, 'b) creator = 'a resource -> string -> ('a, 'b) column

    val int : ('a, int) creator
    val string : ('a, string) creator
    val bool : ('a, bool) creator
    val date : ('a, Date.t) creator
  end
end

module Query : sig
  type 'a t
  type 'a select

  val select : ('a, 'b) column -> 'a select

  type 'a order

  val desc : ?null:[ `First | `Last ] -> ('a, 'b) column -> 'a order
  val asc : ?null:[ `First | `Last ] -> ('a, 'b) column -> 'a order

  type 'a constr

  val ( ! ) : 'a constr -> 'a constr
  val ( && ) : 'a constr -> 'a constr -> 'a constr
  val ( || ) : 'a constr -> 'a constr -> 'a constr

  val create
    :  ?select:'a select list
    -> ?filter:'a constr
    -> ?order:'a order list
    -> ?limit:int
    -> ?offset:int
    -> 'a resource
    -> 'a t

  val to_string : 'a t -> string

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

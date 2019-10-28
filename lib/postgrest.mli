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

module Query : sig
  type 'a column

  val int : string -> int column
  val string : string -> string column
  val bool : string -> bool column
  val date : string -> Date.t column

  type select

  val select : 'a column -> select

  type order

  val desc : ?null:[ `First | `Last ] -> 'a column -> order
  val asc : ?null:[ `First | `Last ] -> 'a column -> order

  type constr

  val not_ : constr -> constr
  val and_ : constr -> constr -> constr
  val or_ : constr -> constr

  val query
    :  ?select:select list
    -> ?filter:constr
    -> ?order:order list
    -> ?limit:int
    -> ?offset:int
    -> string
    -> string

  module type EQUAL = sig
    type t

    val eq : t column -> t -> constr
    val neq : t column -> t -> constr
  end

  module type COMPARE = sig
    type t

    val gt : t column -> t -> constr
    val gte : t column -> t -> constr
    val lt : t column -> t -> constr
    val lte : t column -> t -> constr
  end

  module type TEXT = sig
    type t

    val gt : t column -> t -> constr
    val gte : t column -> t -> constr
    val lt : t column -> t -> constr
    val lte : t column -> t -> constr
  end

  module Bool : sig
    type t = bool

    val is_true : t column -> constr
    val is_false : t column -> constr
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

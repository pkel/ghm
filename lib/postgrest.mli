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

module Column : sig
  type 'a t

  val int : string -> int t
  val string : string -> string t
  val bool : string -> bool t
  val date : string -> Date.t t
end

module Query : sig
  type t
  type select

  val select : 'a Column.t -> select

  type order

  val desc : ?null:[ `First | `Last ] -> 'a Column.t -> order
  val asc : ?null:[ `First | `Last ] -> 'a Column.t -> order

  type constr

  val ( ! ) : constr -> constr
  val ( && ) : constr -> constr -> constr
  val ( || ) : constr -> constr -> constr

  val create
    :  ?select:select list
    -> ?filter:constr
    -> ?order:order list
    -> ?limit:int
    -> ?offset:int
    -> string
    -> t

  val to_string : t -> string

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

type t

external eq : t -> t -> bool = "is" [@@bs.val] [@@bs.scope "Object"]

external int : int -> t = "%identity"

external intify : ('a -> int[@bs]) -> ('a -> t[@bs]) = "%identity"

external str : string -> t = "%identity"

external strify : ('a -> string[@bs]) -> ('a -> t[@bs]) = "%identity"

external as_uniq : t -> Boost.Unique.t = "%identity"

external unsafe_prim_to_addr : 'a -> t = "%identity"

let none_addr : t = [%raw "Symbol.for('boost-inc/none')"]

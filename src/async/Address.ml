type t

external eq : t -> t -> bool = "is" [@@bs.val][@@bs.scope "Object"]

external int : int -> t = "%identity"

external str : string -> t = "%identity"

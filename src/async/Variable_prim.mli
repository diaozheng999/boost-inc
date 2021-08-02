val int : ?name:string -> int -> int Variable.t

val str : ?name:string -> string -> string Variable.t

val bool : ?name:string -> bool -> bool Variable.t

val float : ?name:string -> float -> float Variable.t

val opt : ?name:string -> 'a option -> 'a option Variable.t

val mem : ?name:string -> 'a -> 'a Variable.t

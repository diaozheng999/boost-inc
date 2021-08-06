type 'a t = ((('a -> unit)[@bs]) -> unit[@bs])

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val apply : ('a -> 'b) t -> 'a t -> 'b t

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

module P : sig
  val return : 'a Js.Promise.t -> 'a t

  val map : 'a t -> f:('a -> 'b Js.Promise.t) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val bind : 'a t -> f:('a -> 'b t Js.Promise.t) -> 'b t

  val apply : ('a -> 'b Js.Promise.t) t -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b Js.Promise.t) t -> 'a t -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b Js.Promise.t) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t Js.Promise.t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t Js.Promise.t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b Js.Promise.t) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val act : 'a t -> 'a Js.Promise.t

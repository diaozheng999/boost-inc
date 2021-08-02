type 'a t

val make : mutex:Boost.Mutex.t -> 'a t

val acquire :
  mutex:'a t -> owner:'a -> ((unit -> 'b Js.Promise.t)[@bs]) -> 'b Js.Promise.t

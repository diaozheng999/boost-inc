external arity0 : (unit -> 'a) -> (unit -> 'a[@bs]) = "%identity"

external arity1 : ('a -> 'b) -> ('a -> 'b[@bs]) = "%identity"

external promise_then_0 : ((unit -> 'b Js.Promise.t)[@bs]) -> 'b Js.Promise.t
  = "then"
  [@@bs.send.pipe: unit Js.Promise.t]

external promise_then_1 : (('a -> 'b Js.Promise.t)[@bs]) -> 'b Js.Promise.t
  = "then"
  [@@bs.send.pipe: 'a Js.Promise.t]

external exec_unit_finally : (unit -> unit) -> unit Js.Promise.t = "then"
  [@@bs.send.pipe: unit Js.Promise.t]

external unstable_promise_then_unit_exec : ('a -> 'b) -> 'b Js.Promise.t
  = "then"
  [@@bs.send.pipe: 'a Js.Promise.t]
(** This function utilises the fact that [Promise.then] can take in a void
    function and still return a void promise. This should absolutely not be used
    when creating promises. We use this function internally to avoid creating
    unnecessary [Promise] objects at event handlers. *)

external bind :
  (('a -> 'b)[@bs]) -> (_[@bs.as {json|null|json}]) -> 'a -> (unit -> 'b[@bs])
  = "bind"
  [@@bs.send]

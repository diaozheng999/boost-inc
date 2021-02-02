module Make(Element: Types.Element): Types.Priority_queue
  with type elt = Element.t

module PQ_element: Types.Element
  with type t = (unit -> unit) * Time.window

module M: Types.Priority_queue with type elt = PQ_element.t

type t = M.t

val init: unit -> unit

val insert: M.elt -> unit

val findMin: unit -> M.elt option

val deleteMin: unit -> M.elt

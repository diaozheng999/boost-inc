module type Element = sig
  type t
  val compare: t -> t -> CmpImpl.ord
  val isValid: t -> bool
end

module type Mod = sig
  type elt
  type t
  exception Empty

  val empty: unit -> t
  val isEmpty: t -> bool
  val findMin: t -> elt option * t
  val insert: elt -> t -> t
  val deleteMin: t -> elt * t
end

module Make(Element: Element): Mod with type elt = Element.t

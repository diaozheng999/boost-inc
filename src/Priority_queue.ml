open HeapImpl

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

module Make(Element : Element) = struct

  type elt = Element.t
  type t = (elt, elt) boost_heap
  exception Empty



  let validate elt =
    match elt with
      | Some(n) -> if Element.isValid(n) then Some(n) else None
      | None -> None
    
  let validateNode node =
    match node with
      | Some((n, _)) -> validate (Some(n))
      | None -> None

  let empty () = CmpImpl.abs Element.compare |> initWithComparison

  let isEmpty heap =
    match validateNode (peek heap) with
      | Some(_) -> true
      | None -> false

  let findMin heap = (validateNode (peek heap), heap)
  
  let insert e heap = push ~value:e ~p:e heap; heap
  
  let deleteMin heap =
    match validate (pop heap) with
      | Some(elt) -> (elt, heap)
      | None -> raise Empty
end

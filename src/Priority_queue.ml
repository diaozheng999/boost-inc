open HeapImpl

module Make(Element : Types.Element) = struct

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

  let findMin heap = validate (pop heap)
  
  let insert e heap = push ~value:e ~p:e heap; heap
  
  let deleteMin heap =
    match validate (pop heap) with
      | Some(elt) -> elt
      | None -> raise Empty
end

module PQ_element = struct
  type t = (unit -> unit Js.Promise.t) * Time.t * Time.t
  let compare (_, sa, _) (_, sb, _) = Time.compare sa sb
  let isValid (_, s, _) = not (Time.isSplicedOut s)
end

module M = Make(PQ_element)

type t = M.t

let queue: t ref = ref (M.empty ())

let init () = queue := M.empty ()

let insert e = M.insert e (!queue) |> ignore

let findMin () = M.findMin (!queue)

let deleteMin () = M.deleteMin (!queue)

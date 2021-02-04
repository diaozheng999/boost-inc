open Basis

type timestamp = {
  at: float;
  sub: float;
  mutable isSplicedOut: bool;
}

type t = timestamp LinkedListImpl.boost_linked_list_node
type window = t * t

type __state = { mutable value: timestamp LinkedListImpl.t }

exception BadNode

let list: __state = { value = LinkedListImpl.init () }
  
let now () = { at = Js.Date.now (); sub = 0.; isSplicedOut = false }

let getNext ({ next }: t) = next

let create () =
  let timestamp = now () in
  LinkedListImpl.addToEnd list.value timestamp

let compare (a: t) (b: t) =
  if a == b then Equal else
  match ordFromJs (num a.value.at b.value.at) with
    | Equal -> ordFromJs (num a.value.sub b.value.sub)
    | cmp -> cmp

external __unsafe_inline : float -> string = "%identity"
 
let toString (t: t) = ((__unsafe_inline t.value.at) ^ "|") ^ (__unsafe_inline t.value.sub)

let add (a: t) =
  match a.next with
    | Some(next) ->
      if a.value.at == next.value.at then
        let sub = (a.value.sub +. next.value.sub) /. 2. in
        let timestamp = { at = a.value.at; sub; isSplicedOut = false } in
        LinkedListImpl.addAfter list.value a timestamp
      else
        let at = ((a.value.at +. next.value.at) /. 2.) in
        let timestamp = { at; sub = 0.; isSplicedOut = false } in
        LinkedListImpl.addAfter list.value a timestamp
    | None ->
      let at = Js.Date.now () in
      if at = a.value.at then
        let timestamp = { at; sub = a.value.sub +. 1.; isSplicedOut = false } in
        LinkedListImpl.addToEnd list.value timestamp
      else
        let timestamp = { at; sub = 0.; isSplicedOut = false } in
        LinkedListImpl.addToEnd list.value timestamp

let init () =
  list.value <- LinkedListImpl.init ()

let spliceOut (start: t) (stop: t) =
  let rec deleteRange (next: t) =
    if next == stop then ()
    else
      let nextnext = next.next in
      LinkedListImpl.removeNode list.value next;
      next.value.isSplicedOut <- true;
      match nextnext with
        | Some(n) -> deleteRange n
        | None -> raise BadNode
  in
  match compare start stop with
    | Less -> deleteRange(start)
    | _ -> ()

let isSplicedOut ({ value={ isSplicedOut } }: t) = isSplicedOut

let compareWindow (l, _) (r, _) = compare l r

external toArray: 'a LinkedListImpl.t -> 'a array = "from" [@@bs.val][@@bs.scope "Array"]

let describeTime () = Js.log (toArray list.value)

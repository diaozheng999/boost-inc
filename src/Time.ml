open CmpImpl

type timestamp = {
  at: float;
  mutable isSplicedOut: bool;
}

type t = timestamp LinkedListImpl.boost_linked_list_node

type __state = { mutable value: timestamp LinkedListImpl.t }

exception Order
exception Bad_node

let list: __state = { value = LinkedListImpl.init () }
  
let now () = { at = Js.Date.now (); isSplicedOut = false }

let getNext ({ next }: t) = next

let create () =
  let timestamp = now () in
  LinkedListImpl.addToEnd list.value timestamp

let compare (a: t) (b: t) =
  if a = b then Equal else
  match ordFromJs (num a.value.at b.value.at) with
    | Equal -> 
      let rec compareUp (c: t) ifNone =
        if c.value.at > a.value.at then ifNone a
        else if c = b then Less
        else match c.next with
          | Some c' -> compareUp c' ifNone
          | _ -> raise Order in
      let rec compareDown (c: t) =
        if c.value.at < a.value.at then raise Order
        else if c = b then Greater
        else match c.prev with
          | Some c' -> compareDown c'
          | _ -> raise Order in
      compareUp a compareDown
    | cmp -> cmp

let add (a: t) =
  match a.next with
    | Some(next) ->
      let at = ((a.value.at +. next.value.at) /. 2.) in
      let timestamp = { at; isSplicedOut = false } in
      LinkedListImpl.addAfter list.value a timestamp
    | None -> LinkedListImpl.addToEnd list.value (now ())

let init () =
  list.value <- LinkedListImpl.init ()

let spliceOut (start: t) (stop: t) =
  let rec deleteRange (next: t) =
    if next = stop then ()
    else
      let nextnext = next.next in
      LinkedListImpl.removeNode list.value next;
      next.value.isSplicedOut <- true;
      match nextnext with
        | Some(n) -> deleteRange n
        | None -> raise Bad_node
  in
  match compare start stop with
    | Less -> deleteRange(start)
    | _ -> ()

let isSplicedOut ({ value={ isSplicedOut } }: t) = isSplicedOut

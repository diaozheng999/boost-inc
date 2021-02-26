open Boost.Heap

type elt = (unit -> unit) * Time.window
type t = (elt, elt) heap

let compare (_, sa) (_, sb) = Time.compareWindow sa sb

let isValid (_, (s, _)) = not (Time.isSplicedOut s)

let empty () = (CmpImpl.abs compare) |> make

let queue: t ref = ref (empty ()) [@@unbox]

let init () = queue := empty ()

let validate elt =
  match elt with
    | Some(n) -> if isValid n then Some n else None
    | None -> None

let insert e = push (!queue) e ~p:e 

let findMin () = pop (!queue) |> validate

let deleteMin () = pop (!queue) |> validate |> Belt.Option.getExn

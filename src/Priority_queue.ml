open Yalib.Heap
open Yalib.Traits

type elt = (unit -> unit) * Time.window option
type t = (elt, elt) heap

let compareOpt cmpSome a b =
  match a, b with
    | Some a, Some b -> cmpSome a b
    | None, Some _ -> Less
    | Some _, None -> Greater
    | None, None -> Equal

let compare (_, sa) (_, sb) = compareOpt Time.compareWindow sa sb

let isValid (_, sopt) =
  match sopt with
    | Some (s, _) -> not (Time.isSplicedOut s)
    | None -> true

let empty () = (Yalib.Compare.abs compare) |> make

let queue: t ref = ref (empty ()) [@@unbox]

let init () = queue := empty ()

let validate elt =
  match elt with
    | Some(n) -> if isValid n then Some n else None
    | None -> None

let insert e = push (!queue) e ~p:e 

let findMin () = pop (!queue) |> validate

let deleteMin () = pop (!queue) |> validate |> Belt.Option.getExn

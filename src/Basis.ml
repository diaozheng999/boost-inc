include Boost.Traits

type 'a equality = 'a -> 'a -> bool

let rec eq al bl =
  match (al, bl) with
  | [], [] -> true
  | _ :: _, [] -> false
  | [], _ :: _ -> false
  | a :: a', b :: b' -> if a = b then eq a' b' else false

external toString : 'a -> string = "toString" [@@bs.send]

let ignore' = fun [@bs] _ -> ()

let hash = Boost.Hash.hash

let get_uniq_with_default ?label default_gen =
  let gen = match label with
    | Some label -> Boost.Unique.make_with_label ~label
    | None -> default_gen in
  Boost.Unique.value gen

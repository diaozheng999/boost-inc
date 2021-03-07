include Yalib.Traits

type 'a equality = 'a -> 'a -> bool

let rec eq al bl =
    match (al, bl) with
      | ([], []) -> true
      | (_::_, []) -> false
      | ([], _::_) -> false
      | (a::a', b::b') -> 
        if a = b then eq a' b' else false

external toString: 'a -> string = "toString" [@@bs.send]

let ignore' = fun [@bs] _ -> ()

let hash = Yalib.Hash.hash

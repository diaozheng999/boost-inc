include Boost.Traits

let rec eq al bl =
    match (al, bl) with
      | ([], []) -> true
      | (_::_, []) -> false
      | ([], _::_) -> false
      | (a::a', b::b') -> 
        if a = b then eq a' b' else false

external toString: 'a -> string = "toString" [@@bs.send]

external hash: 'a -> int = "hash" [@@bs.module "boost/dist/esm/common/index.js"]

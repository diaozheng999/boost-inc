open Combinators

type 'a cell = Nil | Cons of 'a * 'a cell modref
type 'a t = 'a cell modref

exception EmptyList

let eq a b =
  match (a, b) with
    | Nil, Nil -> true
    | Cons(ha, ta), Cons(hb, tb) -> Box.eq ha hb && ta = tb
    | _ -> false

let write c = write' eq c

let lengthLessThan n l =
  let write = write' (fun a b -> a = b) in
  let rec f n l =
    if n < 1 then write false
    else l >>= fun c ->
      match c with
        | Nil -> write true
        | Cons(_, t) -> f (n-1) t
  in modref (f n l)

let filter f l =
  let lift = mkLift eq in
  let rec filterM c =
    match c with
      | Nil -> write Nil
      | Cons(h, t) ->
        t >>= fun ct -> lift [Box.indexOf h] ct (fun t ->
          if f h then write (Cons(h, modref (t >>= filterM)))
          else t >>= (fun ct -> filterM ct)
        )
  in modref l >>= filterM

external hash: 'a -> int = "hash" [@@bs.module "boost/common"]

let combine binOp l =
  let halfList l =
    let pairEqual (b1, c1) (b2, c2) = Box.eq b1 b2 && eq c1 c2 in
    let writePair = write' pairEqual in
    let lift = mkLiftCC eq eq in

    let rec half c =
      let rec sumRun v c =
        match c with
          | Nil -> writePair (v, c)
          | Cons(h, t) -> t >>= fun ct ->
            if (hash (Box.indexOf h) = 0) then writePair (binOp v h, ct)
            else sumRun (binOp v h) ct
      in match c with
          | Nil -> write Nil
          | Cons(h, t) -> t >>= fun ct ->
            lift [Box.indexOf h] ct (fun t -> t >>= fun ct ->
              let p = modref (sumRun h ct) in
              p >>= fun (v, ct') -> write (Cons(v, modref (half ct'))))
    in modref (l >>= fun c -> half c)
  in

  let rec comb l =
    lengthLessThan 2 l >>= fun b ->
      if b then l >>= fun c ->
        match c with
          | Nil -> raise EmptyList
          | Cons(h, _) -> Combinators.write h
      else
        comb (halfList l)
  in modref (comb l)

open Combinators

let lift (b: 'b Var.t) ~f =
  let lift = mkLiftCC Box.eq Box.eq in
  let lifted av = lift (Box.indexOf av) av f in
  fun () -> b.modref >>= lifted

let lift2 a b ~f =
  let staged aval = lift b ~f:(f aval) () in
  lift a ~f:staged

let lift3 a b c ~f =
  let staged aval = lift2 b c ~f:(f aval) () in
  lift a ~f:staged

let lift4 a b c d ~f =
  let staged aval = lift3 b c d ~f:(f aval) () in
  lift a ~f:staged

let mkMap (a: 'a Var.t) f =
  let f aref =
    aref >>= fun av ->
      let a = Box.valueOf av in
      write (Box.create (f a))
    in
  lift ~f a ()

let mkMap2 a b f =
  let mapped1 av = 
    let a = Box.valueOf av in
    mkMap b (f a) in
  lift a ~f:(fun aref -> aref >>= mapped1) ()

let mkMap3 a b c f =
  let mapped1 av =
    let a = Box.valueOf av in
    mkMap2 b c (f a) in
  lift a ~f:(fun aref -> aref >>= mapped1) ()

let mkMap4 a b c d f =
  let mapped1 av =
    let a = Box.valueOf av in
    mkMap3 b c d (f a) in
  lift a ~f:(fun aref -> aref >>= mapped1) ()

let mkMap5 a b c d e fn =
  let mapped1 av =
    let a = Box.valueOf av in
    mkMap4 b c d e (fn a) in
  lift a ~f:(fun aref -> aref >>= mapped1) ()

let map a ~f = Var.ofCombinator (mkMap a f)
  
let map2 a b ~f = Var.ofCombinator (mkMap2 a b f)

let map3 a b c ~f = Var.ofCombinator (mkMap3 a b c f)

let map4 a b c d ~f = Var.ofCombinator (mkMap4 a b c d f)

let map5 a b c d e ~f = Var.ofCombinator (mkMap5 a b c d e f)

let bind (a: 'a Var.t) ~f =
  let lift = mkLift Box.eq in
  let bound aref =
    let a = Box.valueOf aref in
    lift (Box.indexOf aref) a f
  in
  read a.modref bound
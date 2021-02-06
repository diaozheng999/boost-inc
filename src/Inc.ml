open Combinators

module Var = Var

let lift (b: 'b Var.t) ~f =
  let lift = mkLiftCC Box.eq Box.eq in
  let lifted av = lift [Box.indexOf av] av f in
  fun () -> b.modref >>= lifted

let map (a: 'a Var.t) ~f =
  let f aref =
      aref >>= fun av ->
        let a = Box.valueOf av in
        write (Box.create (f a))
      in
  let lifted = lift ~f a in
  Var.ofCombinator (lifted ())
  
let map2 (a: 'a Var.t) (b: 'b Var.t) ~f =
  let mapped2 av bv =
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    write (Box.create (f a b))
  in
  let mapped1 av = b.modref >>= mapped2 av in
  Var.ofCombinator (a.modref >>= mapped1)

let map3 (a: 'a Var.t) (b: 'b Var.t) (c: 'c Var.t) ~f =
  let mapped3 av bv cv =
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    let c = Box.valueOf cv in
    write (Box.create (f a b c))
  in
  let mapped2 av bv = c.modref >>= mapped3 av bv in
  let mapped1 av = b.modref >>= mapped2 av in
  Var.ofCombinator (a.modref >>= mapped1)

let map4 (a: 'a Var.t) (b: 'b Var.t) (c: 'c Var.t) (d: 'd Var.t) ~f =
  let mapped4 av bv cv dv =
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    let c = Box.valueOf cv in
    let d = Box.valueOf dv in
    write (Box.create (f a b c d))
  in
  let mapped3 av bv cv = d.modref >>= mapped4 av bv cv in
  let mapped2 av bv = c.modref >>= mapped3 av bv in
  let mapped1 av = b.modref >>= mapped2 av in
  Var.ofCombinator (a.modref >>= mapped1)

open Combinators

module Var = Var

let map ~f (a: 'a Var.t) =
  let mapped av =
      let a = Box.valueOf av in
      write (Box.create (f a)) 
  in
  Var.ofCombinator (a.modref >>= mapped)
  
let map2 ~f (a: 'a Var.t) (b: 'b Var.t) =
  let mapped2 av bv =
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    write (Box.create (f a b))
  in
  let mapped1 av = b.modref >>= mapped2 av in
  Var.ofCombinator (a.modref >>= mapped1)

let map3 ~f (a: 'a Var.t) (b: 'b Var.t) (c: 'c Var.t) =
  let mapped3 av bv cv =
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    let c = Box.valueOf cv in
    write (Box.create (f a b c))
  in
  let mapped2 av bv = c.modref >>= mapped3 av bv in
  let mapped1 av = b.modref >>= mapped2 av in
  Var.ofCombinator (a.modref >>= mapped1)

open Combinators
open Inc

let a = Var.int 1

let b = Var.int 2

let add a b =
  a >>= fun av -> b >>= fun bv ->
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    write (Box.create (a + b))

let out = map2 a b ~f:(fun a b -> a + b)

let c = Var.int 3

let out2 = map2 out c ~f:(fun a b -> a * b)

let _ = Var.log a
let _ = Var.log b
let _ = Var.log out
let _ = Var.log out2

let _ = a.change 2

let _ = Var.log a
let _ = Var.log b
let _ = Var.log out
let _ = Var.log out2

let _ = Meta_.propagate ()

let _ = Var.log a
let _ = Var.log b
let _ = Var.log out
let _ = Var.log out2

let _ = Js.Console.log out
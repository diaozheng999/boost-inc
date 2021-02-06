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

let log () =
  Var.log ~l:"a" a;
  Var.log ~l:"b" b;
  Var.log ~l:"c" c;
  Var.log ~l:"out" out;
  Var.log ~l:"out2" out2

let _ = Js.Console.log "before: a=1 b=2"
let _ = log ()

let _ = a.change 2
let _ = Js.Console.log "a=2 b=2, before propagate"
let _ = log ()

let _ = Meta_.propagate ()

let _ = Js.Console.log "a=2 b=2, after propagate"
let _ = log ()

let _ = a.change 1
let _ = Js.Console.log "a=2 b=2, before propagate"
let _ = log ()
let _ = Meta_.propagate ()
let _ = Js.Console.log "a=2 b=2, after propagate"
let _ = log ()

let out3 = map a ~f:(fun a -> a + 2)

let log' () =
  Var.log ~l:"a" a;
  Var.log ~l:"out3" out3

let _ = Js.Console.log "before: a=1"
let _ = log' ()

let _ = a.changeEagerly 2
let _ = Js.Console.log "after: a=2"
let _ = log' ()

let _ = a.changeEagerly 1
let _ = Js.Console.log "memo: a=1"
let _ = log' ()

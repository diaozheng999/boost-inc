open Combinators

let a = create (Box.create 1)

let b = create (Box.create 2)

let add a b =
  a >>= fun av -> b >>= fun bv ->
    let a = Box.valueOf av in
    let b = Box.valueOf bv in
    write (Box.create (a + b))

let out = modref (add a b)


let _ = log a
let _ = log b
let _ = log out

let _ = Meta_.change a (Box.create 2)

let _ = log a
let _ = log b
let _ = log out

let _ = Meta_.propagate ()

let _ = log a
let _ = log b
let _ = log out

let _ = Js.Console.log out
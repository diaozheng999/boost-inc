type 'a modref = 'a Modifiable.t

let change = Modifiable.change

let change' = Modifiable.change'

let init () =
  Box.init ();
  Modifiable.init ()

let deref = Modifiable.deref

let propagate = Modifiable.propagate

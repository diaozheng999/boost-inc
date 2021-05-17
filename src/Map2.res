open Combinators
open Var

let mk_map = (a, f) => {
  let mapped = (av) => {
    let a = Box.value(av)
    f(a)->Box.create->write
  }
  read(a.modref, mapped)
}

let mk_map_2 = (a, b, f) => {
  let mapped1 = (av) => {
    let a = Box.value(av)
    mk_map(b, f(a))
  }
  read(a.modref, mapped1)
}

let mk_map_3 = (a, b, c, f) => {
    let mapped1 = (av) => {
    let a = Box.value(av)
    mk_map_2(b, c, f(a))
  }
  read(a.modref, mapped1)
}

let mk_map_4 = (a, b, c, d, f) => {
    let mapped1 = (av) => {
    let a = Box.value(av)
    mk_map_3(b, c, d, f(a))
  }
  read(a.modref, mapped1)
}

let mk_map_5 = (a, b, c, d, e, f) => {
    let mapped1 = (av) => {
    let a = Box.value(av)
    mk_map_4(b, c, d, e, f(a))
  }
  read(a.modref, mapped1)
}

let map = (a, ~f) => Var.of_combinator(mk_map(a, f))

let map2 = (a, b, ~f) => Var.of_combinator(mk_map_2(a, b, f))

let map3 = (a, b, c, ~f) => Var.of_combinator(mk_map_3(a, b, c, f))

let map4 = (a, b, c, d, ~f) => Var.of_combinator(mk_map_4(a, b, c, d, f))

let map5 = (a, b, c, d, e, ~f) => Var.of_combinator(mk_map_5(a, b, c, d, e, f))

let bind = (a, ~f) => {
  let bound = (av) => {
    let a = Box.value(av)
    f(a)
  }
  read(a.modref, bound)
}

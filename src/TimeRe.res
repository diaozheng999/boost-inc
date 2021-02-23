open Boost
open Traits
open Types_internal

let compare_ = (. a, b) => {
  if (a == b) {
    Equal
  } else {
    switch ordFromJs(Compare.num(. a.value.at, b.value.at)) {
      | Equal -> ordFromJs(Compare.num(. a.value.sub, b.value.sub))
      | cmp -> cmp
    }
  }
}

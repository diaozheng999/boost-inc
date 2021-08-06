open Boost

let change_gen = Unique.make_with_label ~label:"Async_combinators.change"

let change variable value =
  Propagate.when_not_propagating (fun [@bs] () ->
      Changeable.act
        (Changeable.write ~label:(Unique.value change_gen) value)
        variable)

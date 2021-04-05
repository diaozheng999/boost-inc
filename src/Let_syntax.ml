(** provide Ppx_let binding *)

module Let_syntax = struct

  let return a = Var.create ~label:"let_syntax_return" a

  let map a ~f = Map.map a ~f

  let sub (a: 'a Var.t) ~f = Combinators.(
    Var.ofCombinator (a.modref >>= fun box -> f box.value)
  )

  let both a b = Map.map2 a b ~f:(fun a b -> a, b)

  let bind (a: 'a Var.t) = Combinators.(
    let lift = Combinators.mkLiftCC ~fname:"let_syntax_bind" Box.eq Box.eq in
    let lifted f abox = 
      let addr = Box.indexOf abox in
      let writeBox aref = aref >>= f in
      lift addr abox writeBox in
    fun ~f -> a.modref >>= lifted f |> Var.ofCombinator
  )

  module Open_on_rhs = Combinators
end

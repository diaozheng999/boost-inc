type dispatcher<'a> = (.'a) => unit

type store<'a, 's>

type middleware<'a, 's> = (.store<'a, 's>) => (.dispatcher<'a>) => dispatcher<'a>

type selector<'s, 'a> = (.'s) => 'a

type outcome<'a> =
  | NoOutcome
  | Send('a)
  | Stop('a)

type redux_driver_options<'a> = {
  eq: option<Basis.equality<'a>>,
  label: option<string>,
}

exception Unattached

module type State = {
  type state
  type action
}

module Make: (S: State) => {
  type action<'a>

  let middleware: middleware<S.action, S.state>

  let readState: (.
    selector<S.state, 'a>,
    ~options:option<redux_driver_options<'a>>
  ) => Var.t<'a>

  let dispatch: (Var.t<'a>, (.'a) => S.action) => unit

  let ofAction: (
    (.S.action) => outcome<'a>,
    ~options:option<redux_driver_options<'a>>
  ) => Var.t<'a>

  let change: (Var.t<'a>, 'a) => action<'a>

  let changeEagerly: (Var.t<'a>, 'a) => action<'a>

  let sync: () => action<'a>

  let subscribeTo: (store<S.action, S.state>) => unit
}

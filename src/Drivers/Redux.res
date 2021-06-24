open Basis
open Var

type dispatcher<'a> = (.'a) => unit

type store<'a, 's>

type middleware<'a, 's> = (.store<'a, 's>) => (.dispatcher<'a>) => dispatcher<'a>

exception Unattached

@bs.send external getState: (store<'a, 's>) => 's = "getState"
@bs.send external dispatch: (store<'a, 's>, 'a) => unit = "dispatch"
@bs.send external subscribe: (store<'a, 's>, () => unit) => unit = "subscribe"

type selector<'s, 'a> = (.'s) => 'a

type outcome<'a> =
  | NoOutcome
  | Send('a)
  | Stop('a)

type unit_outcome =
  | N_NoOutcome
  | N_Send
  | N_Stop

type redux_driver_state<'a, 's> = {
  mutable readers: array<('s) => unit>,
  mutable shouldPropagate: bool,
  mutable dispatch: dispatcher<'a>,
  mutable subscribers: array<(.unit) => unit>,
  mutable actionInterceptors: array<('a) => unit_outcome>,
  mutable subscribedToStore: bool,
  mutable getState: () => 's,
}

type redux_driver_options<'a> = {
  eq: option<equality<'a>>,
  label: option<string>,
}

module type State = {
  type state
  type action
}

module Make = (S: State) => {

  type action<'a> =
    | Change({
        \"type": [#Boost_internal_action],
        payload: (Var.t<'a>, 'a),
      })
    | ChangeEagerly({
        \"type": [#Boost_internal_action],
        payload: (Var.t<'a>, 'a),
      })
    | Sync({ \"type": [#Boost_internal_action] })
    | Init({ \"type": [#Boost_internal_action] })
  
  let state: redux_driver_state<S.action, S.state> = {
    readers: [],
    shouldPropagate: false,
    dispatch: ignore',
    subscribers: [],
    actionInterceptors: [],
    subscribedToStore: false,
    getState: () => raise(Unattached)
  }

  external asJsAction: S.action =>  { "type": string, "TAG": option<int> } = "%identity"
  external asInternalAction: { "type": string, "TAG": option<int> } => action<'a> = "%identity"

  external asAction: action<'a> => S.action = "%identity"

  let classify = (action) => {
    let action = asJsAction(action)
    // exposing BS internals here
    if action["type"] == "Boost_internal_action" && action["TAG"] != None {
      Some (asInternalAction(action))
    } else {
      None
    }
  }

  let updateState = (s) => {
    Js.Array2.forEach(state.readers, (f) => f(s))
  }

  let middleware = (.store) => (.next) => {
    Js.Array2.forEach(state.subscribers, (f) => f(.))
    state.subscribers = []
    state.dispatch = (.a) => dispatch(store, a)
    state.getState = () => getState(store)

    if Flags.debug_redux {
      Js.log("Drivers.Redux.middleware: configuration established.")
    }

    (.action) => {
      if Flags.debug_redux {
        Inspect.log1("Driver.Redux.middleware.<lambda>: received action %s", action)
      }

      switch classify(action) {
        | Some(Change({ payload: (var, next) })) => {
          if !state.subscribedToStore {
            Js.Console.warn(
              "Middleware is not subscribed to Store. You need to manually fire " ++
              "the `sync` action for updates to happen."
            )
          }
          var.change(next)
        }
        | Some(ChangeEagerly({ payload: (var, next) })) => var.changeEagerly(next)
        | Some(Sync(_)) => {
          if !state.subscribedToStore {
            getState(store) -> updateState
          }

          if state.shouldPropagate {
            state.shouldPropagate = false
          }

          Modifiable.propagate()
        }
        | Some(Init(_))
        | None => {
          let outcome = Belt.Array.reduce(
            state.actionInterceptors,
            N_NoOutcome,
            (acc, f) => {
              switch (acc, f(action)) {
                | (N_NoOutcome, outcome) => outcome
                | (N_Send, _) => N_Send
                | (_, N_Send) => N_Send
                | _ => N_Stop
              }
            }
          )

          if !state.subscribedToStore {
            getState(store) -> updateState
          }

          if outcome != N_NoOutcome || state.shouldPropagate {
            state.shouldPropagate = false
            Modifiable.propagate()
          }

          if outcome != N_Stop {
            next(.action)
          }
        }
      }
    }
  }

  let mklabel = (options) => {
    switch options {
      | Some({ label: Some(label) }) => label
      | _ => "redux"
    }
  }

  let readState = (.selector, ~options) => {
    let eq = switch options {
      | Some({ eq: Some(eq) }) => Boost.Eq.abs(eq)
      | _ => Boost.Eq.is
    }

    state.dispatch(.Init({ \"type": #Boost_internal_action }) -> asAction)
    let s = state.getState()

    let var = Var.make(~label=mklabel(options), selector(.s))

    let change = (v) => {
      var.change(v)
      state.shouldPropagate = true
    }

    if !state.subscribedToStore {
      Js.Console.warn(
        "Middleware is not subscribed to Store. You need to manually fire " ++
        "the `sync` action for updates to happen."
      )
    }

    Js.Array2.push(state.readers, (state) => {
      let newValue = selector(.state)
      switch Modifiable.deref'(var.modref) {
        | None => change(newValue) 
        | Some(oldBox) => if !eq(.oldBox.value, newValue) {
          change(newValue)
        } 
      }
    }) -> ignore
    var
  }

  let dispatch = (var, action) => {
    let unsub = var.subscribe1(.(.v) => state.dispatch(.action(.v)))
    Js.Array2.push(state.subscribers, unsub) -> ignore
  }

  let ofAction = (action, ~options) => {
    let var = Var.empty(~label=mklabel(options), ())
    
    let eq = switch options {
      | Some({ eq: Some(eq) }) => Boost.Eq.abs(eq)
      | _ => Boost.Eq.is
    }

    let change = (v) => {
      switch Modifiable.deref'(var.modref) {
        | None => var.change(v)
        | Some(b) => if !eq(.Box.valueOf(b), v) {
          var.change(v)
        }
      }
    }

    Js.Array2.push(state.actionInterceptors, (a) => {
      switch action(.a) {
        | NoOutcome => N_NoOutcome
        | Send(b) => {
          change(b)
          N_Send
        }
        | Stop(b) => {
          change(b)
          N_Stop
        }
      }
    }) -> ignore
    var
  }

  let change = (var, next) => Change({
    \"type": #Boost_internal_action,
    payload: (var, next)
  })

  let changeEagerly = (var, next) => ChangeEagerly({
    \"type": #Boost_internal_action,
    payload: (var, next)
  })

  let sync = () => Sync({ \"type": #Boost_internal_action })

  let subscribeTo = (store) => {
    subscribe(store, () => {
      let s = getState(store)
      Js.Array2.forEach(state.readers, (f) => f(s))
      if state.shouldPropagate {
        state.shouldPropagate = false
        Modifiable.propagate()
      }
    })
    state.subscribedToStore = true
  }
}

open Basis

type dispatcher<'a> = (.'a) => unit

type store<'a, 's>

type observer<'a> = {
  next: (.'a) => unit
}

type middleware<'a, 's> = (.store<'a, 's>) => (.dispatcher<'a>) => dispatcher<'a>

type timeout

@bs.send external getState: (store<'a, 's>) => Js.t<'s> = "getState"
@bs.send external dispatch: (store<'a, 's>, 'a) => unit = "dispatch"
@bs.send external subscribe: (store<'a, 's>, observer<'s>) => unit = "subscribe"

@bs.val external setTimeout: (unit => unit, int) => timeout = "setTimeout"
@bs.val external clearTimeout: (timeout) => unit = "clearTimeout"

type selector<'s, 'a> = (.Js.t<'s>) => Var.t<'a>

type outcome<'a> =
  | NoOutcome
  | Send('a)
  | Stop('a)

type unit_outcome =
  | N_NoOutcome
  | N_Send
  | N_Stop

type redux_driver_state<'a, 's> = {
  mutable readers: array<(Js.t<'s>) => unit>,
  mutable shouldPropagate: bool,
  mutable dispatch: dispatcher<'a>,
  mutable subscribers: array<(.unit) => unit>,
  mutable actionInterceptors: array<('a) => unit_outcome>,
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
  
  let state: redux_driver_state<S.action, S.state> = {
    readers: [],
    shouldPropagate: false,
    dispatch: ignore',
    subscribers: [],
    actionInterceptors: [],
  }

  external asJsAction: S.action =>  { "type": string, "TAG": option<int> } = "%identity"
  external asInternalAction: { "type": string, "TAG": option<int> } => action<'a> = "%identity"

  let classify = (action) => {
    let action = asJsAction(action)
    // exposing BS internals here
    if action["type"] == "Boost_internal_action" && action["TAG"] != None {
      Some (asInternalAction(action))
    } else {
      None
    }
  }

  let middleware = (.store) => (.next) => {

    subscribe(store, {
      next: (.s) => {
        Js.Array2.forEach(state.readers, (f) => f(s))
        if state.shouldPropagate {
          state.shouldPropagate = false
          Modifiable.propagate()
        }
      }
    })
    
    Js.Array2.forEach(state.subscribers, (f) => f(.))
    state.subscribers = []
    state.dispatch = next

    (.action) => {
      switch classify(action) {
        | Some(Change({ payload: (var, next) })) => var.change(next)
        | Some(ChangeEagerly({ payload: (var, next) })) => var.changeEagerly(next)
        | Some(Sync(_)) => Modifiable.propagate()
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

          if outcome != N_NoOutcome {
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
    switch options.label {
      | Some(label) => label
      | None => "redux"
    }
  }

  let readState = (.selector, ~options) => {
    let eq = switch options.eq {
      | Some(eq) => Boost.Eq.abs(eq)
      | None => Boost.Eq.is
    }

    let var = Var.empty(~label=mklabel(options), ())

    let change = (v) => {
      var.change(v)
      state.shouldPropagate = true
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

  let dispatch = (cc, action) => {
    let var = Var.ofCombinator(cc)
    let unsub = var.subscribe1(.(.v) => state.dispatch(.action(.v)))
    Js.Array2.push(state.subscribers, unsub) -> ignore
    var
  }

  let ofAction = (action, ~options) => {
    let var = Var.empty(~label=mklabel(options), ())
    
    let eq = switch options.eq {
      | Some(eq) => Boost.Eq.abs(eq)
      | None => Boost.Eq.is
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

  let sync = () => Sync({ \"type": #Boost_internal_action })
}

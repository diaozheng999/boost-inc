open Basis

type dispatcher<'a> = (.'a) => unit

type store<'a, 's>

type observer<'a> = {
  next: (.'a) => unit
}

type middleware<'a, 's> = (.store<'a, 's>) => (.dispatcher<'a>) => dispatcher<'a>

@bs.send external getState: (store<'a, 's>) => Js.t<'s> = "getState"
@bs.send external dispatch: (store<'a, 's>, 'a) => unit = "dispatch"
@bs.send external subscribe: (store<'a, 's>, observer<'s>) => unit = "subscribe"

type untagged_action<'a> = {
  \"type": [#Boost_internal_action],
  payload: 'a,
}

type untagged_unit_action = {
  \"type": [#Boost_internal_action],
}

type selector<'s, 'a> = (.Js.t<'s>) => Var.t<'a>

type redux_driver_state<'a, 's> = {
  mutable readers: array<(Js.t<'s>) => unit>,
  mutable shouldPropagate: bool,
  mutable currentDispatch: (. 'a) => unit,
  mutable currentSubscribers: array<(.unit) => unit>
}

module type State = {
  type state
  type action
}

module Make = (S: State) => {
  
  let state: redux_driver_state<S.action, S.state> = {
    readers: [],
    shouldPropagate: false,
    currentDispatch: ignore',
    currentSubscribers: [],
  }

  let middleware = (.store) => {

    subscribe(store, {
      next: (.s) => {
        Js.Array2.forEach(state.readers, (f) => f(s))
        if state.shouldPropagate {
          state.shouldPropagate = false
          Modifiable.propagate()
        }
      }
    })
    
    (.next) => {
      Js.Array2.forEach(state.currentSubscribers, (f) => f(.))
      state.currentSubscribers = []
      state.currentDispatch = next

      (.action) => {
        next(.action)
      }
    }
  }

  let readState = (.selector, eqf) => {
    let eq = switch eqf {
      | Some(eq) => eq
      | None => Boost.Eq.exec(Boost.Eq.is)
    }

    let var = Var.empty(~label="redux", ())

    let change = (v) => {
      var.change(v)
      state.shouldPropagate = true
    }

    Js.Array2.push(state.readers, (state) => {
      let newValue = selector(.state)
      switch Modifiable.deref'(var.modref) {
        | None => change(newValue) 
        | Some(oldBox) => if eq(oldBox.value, newValue) {
          change(newValue)
        } 
      }
    }) -> ignore
    var
  }

  let dispatch = (cc, action) => {
    let var = Var.ofCombinator(cc)
    let unsub = var.subscribe1(.(.v) => state.currentDispatch(.action(.v)))
    Js.Array2.push(state.currentSubscribers, unsub) -> ignore
    var
  }
}

@module("react")
external useState: ((.()) => 'a) => ('a, (.'a) => unit) = "useState"

@module("react")
external useEffect: (.(.()) => (.()) => unit, array<'a>) => unit = "useEffect"

@module("react")
external useCallback: Js.Fn.arity2<(('a) => 'b, array<'a>, 'a) => 'b> = "useCallback"

@module("react")
external useMemo: (() => 'a, array<'b>) => 'a = "useMemo"

let useVar = (var: Var.t<'a>) => {
  let (value, setValue) = useState(
    (.()) => Modifiable.deref(var.modref) -> Box.valueOf
  )
  useEffect(.(.()) => var.subscribe1(.setValue), [var])
  value
}

let useLazy = (hook, item) => {
  let var = useMemo(item, [])
  hook(var)
}

let useVarLazy = (item) => useLazy(useVar, item)

let useModref = (modref) => {
  let (value, setValue) = useState((.()) => Modifiable.deref(modref))
  useEffect(.(.()) => {
    let observer = Modifiable.attachObserver1(modref, setValue)
    (.()) => Observer.unsub(observer)
  }, [modref])
  value
}

let useCombinator = (comb: Combinators.cc<'a>) => {
  let modref = useMemo(() => Combinators.modref(comb), [])
  useModref(modref)
}

let useCombinatorLazy = (item) => useLazy(useCombinator, item)

let useInc = (comb, ~default) => {
  let (currentVar, setCurrentVar) = useState((.()) => Var.create(default))
  let dispatch = useCallback(.
    (var) => setCurrentVar(.Var.ofCombinator(comb(var))),
    []
  )
  let value = useVar(currentVar)
  (value, dispatch)
}

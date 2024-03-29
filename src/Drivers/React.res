@module("react")
external useState: ((.()) => 'a) => ('a, (.'a) => unit) = "useState"

@module("react")
external useEffect: (.(.()) => (.()) => unit, array<'a>) => unit = "useEffect"

@module("react")
external useCallback: Js.Fn.arity2<(('a) => 'b, array<'a>, 'a) => 'b> = "useCallback"

@module("react")
external useMemo: (() => 'a, array<'b>) => 'a = "useMemo"

let useVar = (var: Var.t<'a>) => {
  let (value, setValue) = useState((.()) => var.deref())
  useEffect(.(.()) => {
    setValue(.var.deref())
    var.subscribe_uncurried(.setValue)
  }, [var])
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
    setValue(.Modifiable.deref(modref))
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
  let (currentVar, setCurrentVar) = useState((.()) => Var.make(default))
  let dispatch = useCallback(.
    (var) => setCurrentVar(.Var.of_combinator(comb(var))),
    []
  )
  let value = useVar(currentVar)
  (value, dispatch)
}

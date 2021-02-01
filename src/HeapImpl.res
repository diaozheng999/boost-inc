type boost_heap<'e, 'p>
type boost_heap_node<'e, 'p> = ('p, 'e)

@bs.new @bs.module("boost/ds")
external init: () => boost_heap<'e, 'p> = "Heap"

@bs.new @bs.module("boost/ds")
external initWithComparison: CmpImpl.abs_compare<'p> => boost_heap<'e, 'p> = "Heap"

@bs.send
external peek: boost_heap<'e, 'p> => option<boost_heap_node<'e, 'p>> = "peek"

@bs.send
external push: (boost_heap<'e, 'p>, ~value: 'e, ~p: 'p) => unit = "push"

@bs.send
external pop: boost_heap<'e, 'p> => option<'e> = "pop"

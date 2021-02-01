type rec boost_linked_list_node<'t> = {
  value: 't,
  prev: option<boost_linked_list_node<'t>>,
  next: option<boost_linked_list_node<'t>>,
}

type boost_linked_list<'a>

type t<'a> = boost_linked_list<'a>
type node<'a> = boost_linked_list_node<'a>

@bs.module("boost/ds") @bs.scope("LinkedList")
external ofArray: array<'a> => t<'a> = "ofArray"

@bs.module("boost/ds") @bs.scope("LinkedList")
external compare: CmpImpl.abs_compare<'a> => CmpImpl.abs_compare<t<'a>> = "compare"

@bs.module("boost/ds") @bs.scope("LinkedList")
external eq: CmpImpl.eq<'a> => CmpImpl.eq<t<'a>> = "eq"

@bs.send external head: t<'a> => option<node<'a>> = "head"
@bs.send external tail: t<'a> => option<node<'a>> = "tail"

@bs.module("boost/ds") @bs.new
external init: () => t<'a> = "LinkedList"

@bs.module("boost/ds") @bs.new
external segment: (~head:node<'a>, ~tail:node<'a>) => t<'a> = "LinkedList"

@bs.send external removeFromFront: t<'a> => option<'a> = "removeFromFront"
@bs.send external removeFromEnd: t<'a> => option<'a> = "removeFromEnd"
@bs.send external addToFront: (t<'a>, 'a) => node<'a> = "addToFront"
@bs.send external addToEnd: (t<'a>, 'a) => node<'a> = "addToEnd"
@bs.send external addBefore: (t<'a>, node<'a>, 'a) => node<'a> = "addBefore"
@bs.send external addAfter: (t<'a>, node<'a>, 'a) => node<'a> = "addAfter"
@bs.send external push: (t<'a>, 'a) => unit = "push"
@bs.send external pop: t<'a> => option<'a> = "pop"
@bs.send external shift: t<'a> => option<'a> = "shift"
@bs.send external unshift: t<'a> => option<'a> = "unshift"

@bs.send external removeNode: (t<'a>, node<'a>) => unit = "removeNode"
@bs.send external joinAfter: (t<'a>, node<'a>, t<'a>) => unit = "joinAfter"
@bs.send external joinBefore: (t<'a>, node<'a>, t<'a>) => unit = "joinBefore"
@bs.send external joinAtEnd: (t<'a>, t<'a>) => unit = "joinAtEnd"
@bs.send external joinAtFront: (t<'a>, t<'a>) => unit = "joinAtFront"

@bs.send external forEach: (t<'a>, ~f:('a => unit)) => unit = "forEach"
@bs.send external forEachNode: (t<'a>, ~f:(node<'a> => unit)) => unit = "forEachNode"

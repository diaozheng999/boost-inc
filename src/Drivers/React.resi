let useVar: (Var.t<'a>) => 'a

let useModref: (Modifiable.t<'a>) => 'a

let useCombinator: (Combinators.cc<'a>) => 'a

let useVarLazy: (() => Var.t<'a>) => 'a

let useCombinatorLazy: (() => Combinators.cc<'a>) => 'a

let useInc: (('a) => Combinators.cc<Box.t<'b>>, ~default:'b) => ('b, ('a) => unit)


@bs.deriving({jsConverter: newType}) type ord =
  | @bs.as(-1) Less
  | @bs.as(0) Equal
  | @bs.as(1) Greater

type abs_compare<'a> = 'a => 'a => abs_ord

type compare<'a> = 'a => 'a => ord

type eq<'a> = 'a => 'a => bool

@bs.module("boost/dist/esm/common/index.js") @bs.scope("compare")
external int: int => int => abs_ord = "int"

@bs.module("boost/dist/esm/common/index.js") @bs.scope("compare")
external num: float => float => abs_ord = "num"

@bs.module("boost/dist/esm/common/index.js") @bs.scope("compare")
external str: string => string => abs_ord = "str"

@bs.module("boost/dist/esm/common/index.js") @bs.scope("compare")
external bool: bool => bool => abs_ord = "bool"

@bs.module("boost/dist/esm/common/index.js") @bs.scope("compare")
external negate: abs_compare<'a> => abs_compare<'a> = "negate"

@bs.module("boost/dist/esm/common/index.js") @bs.scope("clamp")
external clampForComparison: int => abs_ord = "clampForComparison"

@bs.module("boost/dist/esm/common/index.js") @bs.scope("clamp")
external clampFloatForComparison: float => abs_ord = "clampForComparison"

let abs = (f) => (a, b) => ordToJs(f(a, b))

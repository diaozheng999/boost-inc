const Prim = require("../_build/default/src/async/Variable_prim.bs")
const Variable = require("../_build/default/src/async/Variable.bs")
const Memo = require("../_build/default/src/async/Memo.bs")

const Propagate = require("../_build/default/src/async/Propagate.bs")


const a = Prim.$$int("a", 1)
Variable.describe(a)
const f = Memo.mk_map(async (a) => a + 1)


async function main() {
  const b = await f(a)
  Variable.describe(a)
  Variable.describe(b)
  await Variable.write(a, 3)
  Variable.describe(a)
  Variable.describe(b)
  await Propagate.exec()
  console.log(a)
  console.log(b)
}

main()

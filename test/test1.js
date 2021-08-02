const Prim = require("../_build/default/src/async/Variable_prim.bs")
const Variable = require("../_build/default/src/async/Variable.bs")
const Memo = require("../_build/default/src/async/Memo.bs")

const Propagate = require("../_build/default/src/async/Propagate.bs")


const a = Prim.$$int("a", 1)

const f = Memo.mk_map(async (a) => a + 1)


async function main() {
  const b = await f(a)
  console.log(b)
  await Variable.write(a, 3)
  console.log(a)
  console.log(b)
  await Propagate.exec()
  console.log(a)
  console.log(b)
}

main()

const Prim = require("../_build/default/src/async/Variable_prim.bs")
const Variable = require("../_build/default/src/async/Variable.bs")
const Changeable = require("../_build/default/src/async/Changeable.bs")
const Memo = require("../_build/default/src/async/Memo.bs")
const Combinators = require("../_build/default/src/async/Async_combinators.bs")
const Propagate = require("../_build/default/src/async/Propagate.bs")

async function main() {
  try {
    const a = Prim.$$int("a", 1)
    Variable.describe(a)
    const b = await Changeable.variable(
      Changeable.read(undefined, undefined, a, (n) => Changeable.write(undefined, undefined, n + 1))
    );
    const c = await Memo.map(b, async (a) => a + 1);
    Variable.describe(a)
    Variable.describe(b)
    Variable.describe(c)
    await Combinators.change(a, 3)
    Variable.describe(a)
    Variable.describe(b)
    Variable.describe(c)
    await Propagate.exec()
    Variable.describe(a)
    Variable.describe(b)
    Variable.describe(c)
    await Propagate.exec()
    /*
    console.log(a)
    console.log(b)
    */
  } catch (e) {
    console.error(e)
  }
}

main()

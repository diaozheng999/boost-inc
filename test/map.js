const Prim = require("../_build/default/src/async/Variable_prim.bs");
const Memo = require("../_build/default/src/async/Memo.bs");
const Variable = require("../_build/default/src/async/Variable.bs");
const Propagate = require("../_build/default/src/async/Propagate.bs");

async function main() {
  try {
    const a = Prim.$$int("a", 1);
    const b = Memo.map(a, async (a) => a + 1);
    Variable.describe(a);
    Variable.describe(b);
    await Propagate.exec();
    Variable.describe(a);
    Variable.describe(b);
    // await Propagate.exec();
  } catch (e) {
    console.error(e);
  }
}

main();

const Inc = require("../lib/js/src/Inc.bs");
const Var = require("../lib/js/src/Var.bs");
const Modifiable = require("../lib/js/src/Modifiable.bs");
const P = require("../lib/js/src/Promise.bs");

let a = Var.$$int(4);

let p = P.make(new Promise((res) => {
  setTimeout(() => {
    console.log('update');
    res(2);
  }, 200);
}));

let wait = (ms) => new Promise((res) => setTimeout(res, ms))

async function loop() {
  while (true) {
    await wait(2000);
    console.log("sync...");
  }
}

loop();

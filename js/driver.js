const Inc = require("../lib/js/src/Inc.bs");
const Var = require("../lib/js/src/Var.bs");
const Modifiable = require("../lib/js/src/Modifiable.bs");
const P = require("../lib/js/src/Promise.bs");
const Observer = require("../lib/js/src/Observer.bs");

let a = Var.$$int(4);

let p = P.make(new Promise((res) => {
  setTimeout(() => {
    console.log('update');
    res(2);
  }, 5000);
}));

const obs = Modifiable.attachObserver(p.modref, "jsobs", (e) => {
  console.log(`Value updated to`, e);
})
console.log(obs);

let wait = (ms) => new Promise((res) => setTimeout(res, ms))

async function setP() {
  await wait(6000);
  console.log("updating value to 3");
  p.changeEagerly(3);
  await wait(2000);
  console.log("unsubbing", obs);
  Observer.unsub(obs);
  await wait (2000);
  p.changeEagerly(4);
  await wait (2000);
  p.changeEagerly(5);
}

async function loop() {
  setP();
  /*
  while (true) {
    await wait(2000);
    console.log("sync...");
  }*/
}

loop();

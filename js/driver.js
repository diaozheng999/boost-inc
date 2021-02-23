const Inc = require("../lib/js/src/Inc.bs");
const Var = require("../lib/js/src/Var.bs");
const Modifiable = require("../lib/js/src/Modifiable.bs");
const P = require("../lib/js/src/Promise.bs");

let a = Var.$$int(4);

let p = P.make(new Promise((res) => {
  setTimeout(() => res(2), 200);
}));

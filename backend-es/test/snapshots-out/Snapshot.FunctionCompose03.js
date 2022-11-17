import {f, g} from "./foreign.js";
const test4 = /* #__PURE__ */ (() => {
  const $0 = g();
  const $1 = f();
  const $2 = g();
  const $3 = f();
  const $4 = g();
  return x => $0($1($2($3($4(x)))));
})();
const test3 = /* #__PURE__ */ (() => {
  const $0 = f();
  const $1 = g();
  const $2 = f();
  const $3 = g();
  return x => $0($1($2($3(x))));
})();
const test2 = /* #__PURE__ */ (() => {
  const $0 = g();
  const $1 = f();
  const $2 = g();
  return x => $0($1($2(x)));
})();
const test1 = /* #__PURE__ */ (() => {
  const $0 = f();
  const $1 = g();
  return x => $0($1(x));
})();
export {test1, test2, test3, test4};
export * from "./foreign.js";

import {f, g} from "./foreign.js";
const test4 = $0 => $1 => $2 => f($0, $1, $2);
const test3 = /* #__PURE__ */ (() => {
  const $0 = g(1);
  return $1 => $2 => f($0, $1, $2);
})();
const test2 = /* #__PURE__ */ (() => {
  const $0 = g(1);
  return $1 => f($0, 2, $1);
})();
const test1 = /* #__PURE__ */ f(/* #__PURE__ */ g(1), 2, 3);
export {test1, test2, test3, test4};
export * from "./foreign.js";

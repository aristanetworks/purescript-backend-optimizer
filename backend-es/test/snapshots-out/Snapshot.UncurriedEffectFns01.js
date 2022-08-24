import * as $runtime from "../runtime.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import {f, g} from "./foreign.js";
const test6 = /* #__PURE__ */ (() => {
  const $0 = g(1);
  return () => {
    f($0, 2, 3);
    f(g(1), 2, 3);
    return f(g(1), 2, 3);
  };
})();
const test5 = /* #__PURE__ */ (() => {
  const $0 = g(1);
  return () => {
    f($0, 2, 3);
    return f(g(1), 2, 3);
  };
})();
const test4 = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(f);
const test3 = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(f)(/* #__PURE__ */ g(1));
const test2 = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(f)(/* #__PURE__ */ g(1))(2);
const test1 = /* #__PURE__ */ (() => {
  const $0 = g(1);
  return () => f($0, 2, 3);
})();
export {test1, test2, test3, test4, test5, test6};
export * from "./foreign.js";

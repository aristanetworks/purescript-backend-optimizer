import * as $runtime from "../runtime.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Snapshot$dUncurriedEffectFns01$foreign from "./foreign.js";
const f = Snapshot$dUncurriedEffectFns01$foreign.f;
const g = Snapshot$dUncurriedEffectFns01$foreign.g;
const test6 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return () => {
    f(_0, 2, 3);
    f(g(1), 2, 3);
    return f(g(1), 2, 3);
  };
})();
const test5 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return () => {
    f(_0, 2, 3);
    return f(g(1), 2, 3);
  };
})();
const test4 = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(f);
const test3 = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(f)(/* #__PURE__ */ g(1));
const test2 = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(f)(/* #__PURE__ */ g(1))(2);
const test1 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return () => f(_0, 2, 3);
})();
export {f, g, test1, test2, test3, test4, test5, test6};
export * from "./foreign.js";

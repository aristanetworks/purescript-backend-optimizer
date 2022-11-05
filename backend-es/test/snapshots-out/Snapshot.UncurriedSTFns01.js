import * as Control$dMonad$dST$dUncurried from "../Control.Monad.ST.Uncurried/index.js";
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
const test4 = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn3(f);
const test3 = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn3(f)(/* #__PURE__ */ g(1));
const test2 = /* #__PURE__ */ Control$dMonad$dST$dUncurried.runSTFn3(f)(/* #__PURE__ */ g(1))(2);
const test1 = /* #__PURE__ */ (() => {
  const $0 = g(1);
  return () => f($0, 2, 3);
})();
export {test1, test2, test3, test4, test5, test6};
export * from "./foreign.js";

import * as $runtime from "../runtime.js";
import {f, g} from "./foreign.js";
const test4 = _0 => _1 => _2 => f(_0, _1, _2);
const test3 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return _1 => _2 => f(_0, _1, _2);
})();
const test2 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return _1 => f(_0, 2, _1);
})();
const test1 = /* #__PURE__ */ f(/* #__PURE__ */ g(1), 2, 3);
export {test1, test2, test3, test4};
export * from "./foreign.js";

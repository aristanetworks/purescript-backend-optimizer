import * as $runtime from "../runtime.js";
import {g, r} from "./foreign.js";
const test9 = /* #__PURE__ */ (() => {
  const _0 = g(42);
  return () => {
    const ref = {value: _0};
    const prev = ref.value;
    ref.value = prev + 1 | 0;
    const _4 = ref.value;
    ref.value = 1 + _4 | 0;
    return ref.value;
  };
})();
const test8 = /* #__PURE__ */ (() => {
  const _0 = g(g);
  return () => {
    const _1 = r.value;
    return r.value = _0(_1);
  };
})();
const test7 = () => {
  const _0 = r.value;
  return r.value = g(_0);
};
const test6 = /* #__PURE__ */ (() => {
  const _0 = g(42);
  return () => r.value = _0;
})();
const test5 = () => r.value = 42;
const test4 = /* #__PURE__ */ (() => {
  const _0 = g(r);
  return () => _0.value;
})();
const test3 = () => r.value;
const test2 = /* #__PURE__ */ (() => {
  const _0 = g(42);
  return () => ({value: _0});
})();
const test1 = () => ({value: 42});
export {test1, test2, test3, test4, test5, test6, test7, test8, test9};
export * from "./foreign.js";

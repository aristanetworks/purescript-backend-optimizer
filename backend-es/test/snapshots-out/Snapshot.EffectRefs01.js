import {g, r} from "./foreign.js";
const test9 = /* #__PURE__ */ (() => {
  const $0 = g(42);
  return () => {
    let ref = $0;
    const prev = ref;
    ref = prev + 1 | 0;
    const $1 = ref;
    ref = 1 + $1 | 0;
    return ref;
  };
})();
const test8 = /* #__PURE__ */ (() => {
  const $0 = g(g);
  return () => {
    const $1 = r.value;
    return r.value = $0($1);
  };
})();
const test7 = () => {
  const $0 = r.value;
  return r.value = g($0);
};
const test6 = /* #__PURE__ */ (() => {
  const $0 = g(42);
  return () => r.value = $0;
})();
const test5 = () => r.value = 42;
const test4 = /* #__PURE__ */ (() => {
  const $0 = g(r);
  return () => $0.value;
})();
const test3 = () => r.value;
const test2 = /* #__PURE__ */ (() => {
  const $0 = g(42);
  return () => ({value: $0});
})();
const test1 = () => ({value: 42});
export {test1, test2, test3, test4, test5, test6, test7, test8, test9};
export * from "./foreign.js";

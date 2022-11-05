import {a} from "./foreign.js";
const test2 = /* #__PURE__ */ (() => {
  const $0 = a + 1 | 0;
  return () => $0;
})();
const test1 = () => 1;
export {test1, test2};
export * from "./foreign.js";

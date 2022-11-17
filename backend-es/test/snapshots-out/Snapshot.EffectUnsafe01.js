import {random} from "./foreign.js";
const test2 = /* #__PURE__ */ (() => {
  const n = random();
  const m = random();
  return n + m | 0;
})();
const test1 = 1;
export {test1, test2};
export * from "./foreign.js";

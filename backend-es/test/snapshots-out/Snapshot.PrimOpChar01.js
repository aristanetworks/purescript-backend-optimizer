import {a, b} from "./foreign.js";
const test6 = /* #__PURE__ */ (() => a >= b)();
const test5 = /* #__PURE__ */ (() => a <= b)();
const test4 = /* #__PURE__ */ (() => a > b)();
const test3 = /* #__PURE__ */ (() => a < b)();
const test2 = /* #__PURE__ */ (() => a !== b)();
const test1 = /* #__PURE__ */ (() => a === b)();
export {test1, test2, test3, test4, test5, test6};
export * from "./foreign.js";

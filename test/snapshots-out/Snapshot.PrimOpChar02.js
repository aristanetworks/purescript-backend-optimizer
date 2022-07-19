import * as $runtime from "./runtime.js";
const charValues = op => [op('a')('a'), op('a')('b'), op('b')('a')];
const test1 = /* #__PURE__ */ (() => [true, false, false])();
const test2 = /* #__PURE__ */ (() => [false, true, true])();
const test3 = /* #__PURE__ */ (() => [false, true, false])();
const test4 = /* #__PURE__ */ (() => [false, false, true])();
const test5 = /* #__PURE__ */ (() => [true, true, false])();
const test6 = /* #__PURE__ */ (() => [true, false, true])();
export {charValues, test1, test2, test3, test4, test5, test6};

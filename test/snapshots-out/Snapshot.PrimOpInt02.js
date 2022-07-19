import * as $runtime from "./runtime.js";
import * as Data$dEuclideanRing from "./Data.EuclideanRing.js";
const test11 = /* #__PURE__ */ (() => [-1, 1])();
const intValues = op => [op(1)(1), op(1)(2), op(2)(1), op(1)(-2), op(-1)(2), op(-1)(-1)];
const test1 = /* #__PURE__ */ (() => [2, 3, 3, -1, 1, -2])();
const test10 = /* #__PURE__ */ (() => [
  Data$dEuclideanRing.intDiv(1)(1),
  Data$dEuclideanRing.intDiv(1)(2),
  Data$dEuclideanRing.intDiv(2)(1),
  Data$dEuclideanRing.intDiv(1)(-2),
  Data$dEuclideanRing.intDiv(-1)(2),
  Data$dEuclideanRing.intDiv(-1)(-1)
])();
const test2 = /* #__PURE__ */ (() => [0, -1, 1, 3, -3, 0])();
const test3 = /* #__PURE__ */ (() => [true, false, false, false, false, true])();
const test4 = /* #__PURE__ */ (() => [false, true, true, true, true, false])();
const test5 = /* #__PURE__ */ (() => [false, true, false, false, true, false])();
const test6 = /* #__PURE__ */ (() => [false, false, true, true, false, false])();
const test7 = /* #__PURE__ */ (() => [true, true, false, false, true, true])();
const test8 = /* #__PURE__ */ (() => [true, false, true, true, false, true])();
const test9 = /* #__PURE__ */ (() => [1, 2, 2, -2, -2, 1])();
export {intValues, test1, test10, test11, test2, test3, test4, test5, test6, test7, test8, test9};

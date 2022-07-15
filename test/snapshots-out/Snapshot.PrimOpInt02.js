import * as Data$dEuclideanRing from "./Data.EuclideanRing.js";
const test11 = [-1, 1];
const intValues = op => [op(1)(1), op(1)(2), op(2)(1), op(1)(-2), op(-1)(2), op(-1)(-1)];
const test1 = [2, 3, 3, -1, 1, -2];
const test10 = [
  Data$dEuclideanRing.intDiv(1)(1),
  Data$dEuclideanRing.intDiv(1)(2),
  Data$dEuclideanRing.intDiv(2)(1),
  Data$dEuclideanRing.intDiv(1)(-2),
  Data$dEuclideanRing.intDiv(-1)(2),
  Data$dEuclideanRing.intDiv(-1)(-1)
];
const test2 = [0, -1, 1, 3, -3, 0];
const test3 = [true, false, false, false, false, true];
const test4 = [false, true, true, true, true, false];
const test5 = [false, true, false, false, true, false];
const test6 = [false, false, true, true, false, false];
const test7 = [true, true, false, false, true, true];
const test8 = [true, false, true, true, false, true];
const test9 = [1, 2, 2, -2, -2, 1];
export {intValues, test1, test10, test11, test2, test3, test4, test5, test6, test7, test8, test9};
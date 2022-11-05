import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import {b} from "./foreign.js";
const test1 = test1$a0$copy => {
  let test1$a0 = test1$a0$copy, test1$c = true, test1$r;
  while (test1$c) {
    const arr = test1$a0;
    const v = Data$dArray.index(arr)(arr.length - 1 | 0);
    const v1 = Data$dArray.index(arr)(0);
    const $3 = (x, y) => {
      if (b) {
        test1$c = false;
        test1$r = [];
        return;
      }
      test1$a0 = Data$dSemigroup.concatArray([y, x, 3, y, 5, 6, 7, 8, 9, 10, x, 12, 13, 14, 15, 16, 17])(arr);
      return;
    };
    if (v.tag === "Just") {
      if (v._1 === 2) {
        if (v1.tag === "Just") {
          if (v1._1 === 1) {
            test1$c = false;
            test1$r = arr;
            continue;
          }
          $3(v1._1, v._1);
          continue;
        }
        if (v1.tag === "Nothing") {
          test1$c = false;
          test1$r = Data$dSemigroup.concatArray(arr)([v._1]);
          continue;
        }
        $runtime.fail();
      }
      if (v1.tag === "Nothing") {
        test1$c = false;
        test1$r = Data$dSemigroup.concatArray(arr)([v._1]);
        continue;
      }
      if (v1.tag === "Just") {
        $3(v1._1, v._1);
        continue;
      }
      $runtime.fail();
    }
    if (v.tag === "Nothing") {
      if (v1.tag === "Nothing") {
        test1$c = false;
        test1$r = arr;
        continue;
      }
      if (v1.tag === "Just") {
        test1$c = false;
        test1$r = Data$dSemigroup.concatArray(arr)([v1._1]);
        continue;
      }
      $runtime.fail();
    }
    $runtime.fail();
  }
  return test1$r;
};
export {test1};
export * from "./foreign.js";

import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
const test1 = test1$a0$copy => test1$a1$copy => {
  let test1$a0 = test1$a0$copy, test1$a1 = test1$a1$copy, test1$c = true, test1$r;
  while (test1$c) {
    const b = test1$a0, arr = test1$a1;
    const v = Data$dArray.last(arr);
    if (0 < arr.length) {
      const $0 = (x, y) => {
        if (b) {
          test1$c = false;
          test1$r = [];
          return;
        }
        test1$a0 = b;
        test1$a1 = Data$dSemigroup.concatArray([y, x, 3, y, 5, 6, 7, 8, 9, 10, x, 12, 13, 14, 15, 16, 17])(arr);
      };
      if (v.tag === "Just") {
        if (v._1 === 2) {
          if (arr[0] === 1) {
            test1$c = false;
            test1$r = arr;
            continue;
          }
          $0(arr[0], v._1);
          continue;
        }
        $0(arr[0], v._1);
        continue;
      }
      if (v.tag === "Nothing") {
        test1$c = false;
        test1$r = Data$dSemigroup.concatArray(arr)([arr[0]]);
        continue;
      }
      $runtime.fail();
    }
    if (v.tag === "Just") {
      if (v._1 === 2) {
        test1$c = false;
        test1$r = Data$dSemigroup.concatArray(arr)([v._1]);
        continue;
      }
      test1$c = false;
      test1$r = Data$dSemigroup.concatArray(arr)([v._1]);
      continue;
    }
    if (v.tag === "Nothing") {
      test1$c = false;
      test1$r = arr;
      continue;
    }
    $runtime.fail();
  }
  return test1$r;
};
export {test1};

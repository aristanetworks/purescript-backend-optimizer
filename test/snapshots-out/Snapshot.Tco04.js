import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dRing from "../Data.Ring/index.js";
const test2test1 = test2test1$b$copy => test2test1$0$copy => test2test1$1$copy => {
  let test2test1$b = test2test1$b$copy;
  let test2test1$0 = test2test1$0$copy;
  let test2test1$1 = test2test1$1$copy;
  let test2test1$c = true;
  let test2test1$r;
  while (test2test1$c) {
    if (test2test1$b === 0) {
      const m = test2test1$0;
      if (m === 2) {
        test2test1$c = false;
        test2test1$r = m;
        continue;
      }
      test2test1$b = 1;
      test2test1$0 = m - 2 | 0;
      continue;
    }
    if (test2test1$b === 1) {
      const n = test2test1$0;
      if (n === 1) {
        test2test1$c = false;
        test2test1$r = n;
        continue;
      }
      test2test1$b = 0;
      test2test1$0 = n - 1 | 0;
      continue;
    }
  };
  return test2test1$r;
};
const test2 = /* #__PURE__ */ test2test1(0);
const test1 = /* #__PURE__ */ test2test1(1);
export {test1, test2};

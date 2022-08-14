import * as $runtime from "../runtime.js";
const test = test$0$copy => {
  let test$0 = test$0$copy, test$c = true, test$r;
  while (test$c) {
    const n = test$0;
    if (n === 0) {
      test$c = false;
      test$r = n;
      continue;
    }
    test$0 = n - 1 | 0;
    continue;
  };
  return test$r;
};
export {test};

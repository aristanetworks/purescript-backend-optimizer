import * as $runtime from "../runtime.js";
const $test2test1 = ($test2test1$b$copy, $test2test1$a0$copy) => {
  let $test2test1$b = $test2test1$b$copy, $test2test1$a0 = $test2test1$a0$copy, $test2test1$c = true, $test2test1$r;
  while ($test2test1$c) {
    if ($test2test1$b === 0) {
      const m = $test2test1$a0;
      if (m === 2) {
        $test2test1$c = false;
        $test2test1$r = m;
        continue;
      }
      $test2test1$b = 1;
      $test2test1$a0 = m - 2 | 0;
      continue;
    }
    if ($test2test1$b === 1) {
      const n = $test2test1$a0;
      if (n === 1) {
        $test2test1$c = false;
        $test2test1$r = n;
        continue;
      }
      $test2test1$b = 0;
      $test2test1$a0 = n - 1 | 0;
      continue;
    }
  };
  return $test2test1$r;
};
const test2 = m => $test2test1(0, m);
const test1 = n => $test2test1(1, n);
export {test1, test2};

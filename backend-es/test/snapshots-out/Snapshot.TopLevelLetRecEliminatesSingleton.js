const $test$test = ($test$test$b$copy, $test$test$a0$copy) => {
  let $test$test$b = $test$test$b$copy, $test$test$a0 = $test$test$a0$copy, $test$test$c = true, $test$test$r;
  while ($test$test$c) {
    if ($test$test$b === 0) {
      const m = $test$test$a0;
      if (m === 100) {
        $test$test$b = 1;
        $test$test$a0 = m - 1 | 0;
        continue;
      }
      if (m === 900) {
        $test$test$c = false;
        $test$test$r = 42;
        continue;
      }
      $test$test$b = 0;
      $test$test$a0 = m - 1 | 0;
      continue;
    }
    if ($test$test$b === 1) {
      const n = $test$test$a0;
      if (n === 0) {
        $test$test$c = false;
        $test$test$r = n;
        continue;
      }
      if (n <= 100) {
        $test$test$b = 1;
        $test$test$a0 = n - 1 | 0;
        continue;
      }
      $test$test$b = 0;
      $test$test$a0 = n - 1 | 0;
    }
  }
  return $test$test$r;
};
const test$0$k$$rec = m => $test$test(0, m);
const test = n => $test$test(1, n);
export {test, test$0$k$$rec};

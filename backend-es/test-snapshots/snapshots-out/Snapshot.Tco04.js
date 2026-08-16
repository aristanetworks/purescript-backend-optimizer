const $tco$test2test1 = ($tco$test2test1$b$copy, $tco$test2test1$a0$copy) => {
  let $tco$test2test1$b = $tco$test2test1$b$copy, $tco$test2test1$a0 = $tco$test2test1$a0$copy, $tco$test2test1$c = true, $tco$test2test1$r;
  while ($tco$test2test1$c) {
    if ($tco$test2test1$b === 0) {
      const m = $tco$test2test1$a0;
      if (m === 2) {
        $tco$test2test1$c = false;
        $tco$test2test1$r = m;
        continue;
      }
      $tco$test2test1$b = 1;
      $tco$test2test1$a0 = m - 2 | 0;
      continue;
    }
    if ($tco$test2test1$b === 1) {
      const n = $tco$test2test1$a0;
      if (n === 1) {
        $tco$test2test1$c = false;
        $tco$test2test1$r = n;
        continue;
      }
      const $0 = n - 1 | 0;
      if ($0 === 2) {
        $tco$test2test1$c = false;
        $tco$test2test1$r = $0;
        continue;
      }
      $tco$test2test1$b = 1;
      $tco$test2test1$a0 = $0 - 2 | 0;
    }
  }
  return $tco$test2test1$r;
};
const test2 = m => $tco$test2test1(0, m);
const test1 = n => $tco$test2test1(1, n);
export {test1, test2};

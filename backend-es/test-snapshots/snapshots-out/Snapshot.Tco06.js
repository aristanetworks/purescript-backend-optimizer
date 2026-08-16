const $tco$gf = ($tco$gf$b$copy, $tco$gf$a0$copy, $tco$gf$a1$copy) => {
  let $tco$gf$b = $tco$gf$b$copy, $tco$gf$a0 = $tco$gf$a0$copy, $tco$gf$a1 = $tco$gf$a1$copy, $tco$gf$c = true, $tco$gf$r;
  while ($tco$gf$c) {
    if ($tco$gf$b === 0) {
      const a = $tco$gf$a0;
      $tco$gf$b = 1;
      $tco$gf$a0 = a;
      $tco$gf$a1 = a + 1 | 0;
      continue;
    }
    if ($tco$gf$b === 1) {
      const a = $tco$gf$a0, b = $tco$gf$a1;
      const $0 = a + b | 0;
      $tco$gf$b = 1;
      $tco$gf$a0 = $0;
      $tco$gf$a1 = $0 + 1 | 0;
    }
  }
  return $tco$gf$r;
};
const g = a => $tco$gf(0, a);
const f = a => b => $tco$gf(1, a, b);
export {f, g};

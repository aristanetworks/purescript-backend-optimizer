const $gf$djf = ($gf$djf$b$copy, $gf$djf$a0$copy, $gf$djf$a1$copy) => {
  let $gf$djf$b = $gf$djf$b$copy, $gf$djf$a0 = $gf$djf$a0$copy, $gf$djf$a1 = $gf$djf$a1$copy, $gf$djf$c = true, $gf$djf$r;
  while ($gf$djf$c) {
    if ($gf$djf$b === 0) {
      const n = $gf$djf$a0;
      $gf$djf$b = 2;
      $gf$djf$a0 = n;
      $gf$djf$a1 = n - 1 | 0;
      continue;
    }
    if ($gf$djf$b === 1) {
      const n = $gf$djf$a0;
      if (n === 100) {
        $gf$djf$b = 2;
        $gf$djf$a0 = n;
        $gf$djf$a1 = n - 1 | 0;
        continue;
      }
      $gf$djf$b = 1;
      $gf$djf$a0 = n + 1 | 0;
      continue;
    }
    if ($gf$djf$b === 2) {
      const a = $gf$djf$a0, b = $gf$djf$a1;
      $gf$djf$b = 1;
      $gf$djf$a0 = a + b | 0;
    }
  }
  return $gf$djf$r;
};
const g = n => $gf$djf(0, n);
const f$dj = n => $gf$djf(1, n);
const f = a => b => $gf$djf(2, a, b);
export {f, f$dj, g};

const test$0$go = n => {
  const k = k$a0$copy => {
    let k$a0 = k$a0$copy, k$c = true, k$r;
    while (k$c) {
      const m = k$a0;
      if (m === 100) {
        k$c = false;
        k$r = test$0$go(m - 1 | 0);
        continue;
      }
      if (m === 900) {
        k$c = false;
        k$r = 42;
        continue;
      }
      k$a0 = m - 1 | 0;
    }
    return k$r;
  };
  if (n === 0) { return n; }
  if (n <= 100) { return test$0$go(n - 1 | 0); }
  return k(n - 1 | 0);
};
const test = test$0$go;
export {test, test$0$go};

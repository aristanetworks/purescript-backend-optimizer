const test$0$go$$rec = test$0$go$$rec$a0$copy => {
  let test$0$go$$rec$a0 = test$0$go$$rec$a0$copy, test$0$go$$rec$c = true, test$0$go$$rec$r;
  while (test$0$go$$rec$c) {
    const n = test$0$go$$rec$a0;
    const k = k$a0$copy => {
      let k$a0 = k$a0$copy, k$c = true, k$r;
      while (k$c) {
        const m = k$a0;
        if (m === 100) {
          k$c = false;
          test$0$go$$rec$a0 = m - 1 | 0;
          continue;
        }
        if (m === 900) {
          test$0$go$$rec$c = k$c = false;
          test$0$go$$rec$r = 42;
          continue;
        }
        k$a0 = m - 1 | 0;
      }
      return k$r;
    };
    if (n === 0) {
      test$0$go$$rec$c = false;
      test$0$go$$rec$r = n;
      continue;
    }
    if (n <= 100) {
      test$0$go$$rec$a0 = n - 1 | 0;
      continue;
    }
    k(n - 1 | 0);
  }
  return test$0$go$$rec$r;
};
const test = test$0$go$$rec;
export {test, test$0$go$$rec};

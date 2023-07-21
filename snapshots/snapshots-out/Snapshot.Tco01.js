const test = test$a0$copy => {
  let test$a0 = test$a0$copy, test$c = true, test$r;
  while (test$c) {
    const n = test$a0;
    if (n === 0) {
      test$c = false;
      test$r = n;
      continue;
    }
    test$a0 = n - 1 | 0;
  }
  return test$r;
};
export {test};

const test1 = test1$a0$copy => test1$a1$copy => {
  let test1$a0 = test1$a0$copy, test1$a1 = test1$a1$copy, test1$c = true, test1$r;
  while (test1$c) {
    const b = test1$a0, arr = test1$a1;
    const $0 = arr.length - 1 | 0;
    if ($0 >= 0 && $0 < arr.length) {
      if (0 < arr.length) {
        if (arr[$0] === 2 && arr[0] === 1) {
          test1$c = false;
          test1$r = arr;
          continue;
        }
        const $1 = arr[$0];
        if (b) {
          test1$c = false;
          test1$r = [];
          continue;
        }
        test1$a0 = b;
        test1$a1 = [$1, arr[0], 3, $1, 5, 6, 7, 8, 9, 10, arr[0], 12, 13, 14, 15, 16, 17, ...arr];
        continue;
      }
      test1$c = false;
      test1$r = [...arr, arr[$0]];
      continue;
    }
    if (0 < arr.length) {
      test1$c = false;
      test1$r = [...arr, arr[0]];
      continue;
    }
    test1$c = false;
    test1$r = arr;
  }
  return test1$r;
};
export {test1};

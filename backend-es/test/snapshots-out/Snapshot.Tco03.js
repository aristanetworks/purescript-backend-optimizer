const test = /* #__PURE__ */ (() => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const n = go$a0;
      const k = k$a0$copy => {
        let k$a0 = k$a0$copy, k$c = true, k$r;
        while (k$c) {
          const m = k$a0;
          if (m === 100) {
            k$c = false;
            go$a0 = m - 1 | 0;
            continue;
          }
          if (m === 900) {
            go$c = k$c = false;
            go$r = 42;
            continue;
          }
          k$a0 = m - 1 | 0;
          continue;
        }
        return k$r;
      };
      if (n === 0) {
        go$c = false;
        go$r = n;
        continue;
      }
      if (n <= 100) {
        go$a0 = n - 1 | 0;
        continue;
      }
      k(n - 1 | 0);
      continue;
    }
    return go$r;
  };
  return go;
})();
export {test};

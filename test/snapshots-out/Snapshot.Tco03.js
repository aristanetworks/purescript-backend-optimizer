import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dRing from "../Data.Ring/index.js";
const test = /* #__PURE__ */ (() => {
  const go = go$0$copy => {
    let go$0 = go$0$copy;
    let go$c = true;
    let go$r;
    while (go$c) {
      const n = go$0;
      const k = k$0$copy => {
        let k$0 = k$0$copy;
        let k$c = true;
        let k$r;
        while (k$c) {
          const m = k$0;
          if (m === 100) {
            k$c = false;
            go$0 = m - 1 | 0;
            continue;
          }
          if (m === 900) {
            go$c = false;
            k$c = false;
            go$r = 42;
            continue;
          }
          k$0 = m - 1 | 0;
          continue;
        };
        return k$r;
      };
      if (n === 0) {
        go$c = false;
        go$r = n;
        continue;
      }
      if (n <= 100) {
        go$0 = n - 1 | 0;
        continue;
      }
      k(n - 1 | 0);
      continue;
    };
    return go$r;
  };
  return go;
})();
export {test};

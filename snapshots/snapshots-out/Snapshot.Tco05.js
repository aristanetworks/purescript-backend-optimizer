import * as Data$dMaybe from "../Data.Maybe/index.js";
const span = p => arr => {
  const go = go$a0$copy => {
    let go$a0 = go$a0$copy, go$c = true, go$r;
    while (go$c) {
      const i = go$a0;
      if (i >= 0 && i < arr.length) {
        if (p(arr[i])) {
          go$a0 = i + 1 | 0;
          continue;
        }
        go$c = false;
        go$r = Data$dMaybe.$Maybe("Just", i);
        continue;
      }
      go$c = false;
      go$r = Data$dMaybe.Nothing;
    }
    return go$r;
  };
  return go(0);
};
export {span};

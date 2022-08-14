import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const span = p => arr => {
  const go = go$0$copy => {
    let go$0 = go$0$copy, go$c = true, go$r;
    while (go$c) {
      const i = go$0;
      const v = Data$dArray.index(arr)(i);
      if (v.tag === "Just") {
        if (p(v._1)) {
          go$0 = i + 1 | 0;
          continue;
        }
        go$c = false;
        go$r = Data$dMaybe.$Maybe("Just", i);
        continue;
      }
      if (v.tag === "Nothing") {
        go$c = false;
        go$r = Data$dMaybe.Nothing;
        continue;
      }
      $runtime.fail();
    };
    return go$r;
  };
  return go(0);
};
export {span};

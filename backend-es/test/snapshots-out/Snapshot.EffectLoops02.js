import * as $runtime from "../runtime.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test4 = lo => hi => () => {
  for (const a of $runtime.range(lo, hi)) {
    if (a < 10) {
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    } else {
      Effect$dConsole.log("wat")();
    }
  }
};
const test3 = lo => hi => () => {
  for (const a of $runtime.range(lo, hi)) {
    const $0 = Effect$dConsole.log(Data$dShow.showIntImpl(a));
    if (a < 10) { $0(); }
  }
};
const test2 = lo => hi => {
  const $0 = lo + 1 | 0;
  const $1 = hi + 1 | 0;
  return () => {
    for (const a of $runtime.range($0, $1)) {
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    }
    for (const $2 of $runtime.range(lo + 1 | 0, hi + 1 | 0)) {
      Effect$dConsole.log(Data$dShow.showIntImpl($2))();
    }
    const $2 = Effect$dConsole.log("wat");
    for (const v of $runtime.range(lo + 1 | 0, hi + 1 | 0)) {
      $2();
    }
  };
};
const test1 = lo => hi => {
  const $0 = lo + 1 | 0;
  const $1 = hi + 1 | 0;
  return () => {
    for (const a of $runtime.range($0, $1)) {
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    }
  };
};
export {test1, test2, test3, test4};

import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test4 = arr => () => {
  for (const a of arr) {
    if (a < 10) {
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    } else {
      Effect$dConsole.log("wat")();
    }
  }
};
const test3 = arr => () => {
  for (const a of arr) {
    const $0 = Effect$dConsole.log(Data$dShow.showIntImpl(a));
    if (a < 10) { $0(); }
  }
};
const test2 = k => {
  const $0 = k(42);
  return () => {
    for (const a of $0) {
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    }
    for (const $1 of k(42)) {
      Effect$dConsole.log(Data$dShow.showIntImpl($1))();
    }
    const $1 = Effect$dConsole.log("wat");
    for (const $2 of k(42)) {
      $1();
    }
  };
};
const test1 = k => {
  const $0 = k(42);
  return () => {
    for (const a of $0) {
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
      Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    }
  };
};
export {test1, test2, test3, test4};

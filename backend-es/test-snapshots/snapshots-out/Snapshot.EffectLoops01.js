import * as $mData$dShow from "../Data.Show/index.js";
import * as $mEffect$dConsole from "../Effect.Console/index.js";
const test4 = arr => () => {
  for (const a of arr) {
    if (a < 10) {
      $mEffect$dConsole.log($mData$dShow.showIntImpl(a))();
    } else {
      $mEffect$dConsole.log("wat")();
    }
  }
};
const test3 = arr => () => {
  for (const a of arr) {
    const $0 = $mEffect$dConsole.log($mData$dShow.showIntImpl(a));
    if (a < 10) { $0(); }
  }
};
const test2 = k => {
  const $0 = k(42);
  return () => {
    for (const a of $0) {
      $mEffect$dConsole.log($mData$dShow.showIntImpl(a))();
    }
    for (const $1 of k(42)) {
      $mEffect$dConsole.log($mData$dShow.showIntImpl($1))();
    }
    const $1 = $mEffect$dConsole.log("wat");
    for (const $2 of k(42)) {
      $1();
    }
  };
};
const test1 = k => {
  const $0 = k(42);
  return () => {
    for (const a of $0) {
      $mEffect$dConsole.log($mData$dShow.showIntImpl(a))();
      $mEffect$dConsole.log($mData$dShow.showIntImpl(a))();
    }
  };
};
export {test1, test2, test3, test4};

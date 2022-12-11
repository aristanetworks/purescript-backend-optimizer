import * as Effect$dConsole from "../Effect.Console/index.js";
const test4 = cond => ref => () => {
  while (cond.value) {
    const a = ref.value;
    if (a < 10) {
      Effect$dConsole.log("foo")();
      continue;
    }
    Effect$dConsole.log("wat")();
  }
};
const test3 = cond => ref => () => {
  while (cond.value) {
    const a = ref.value;
    const $0 = Effect$dConsole.log("foo");
    if (a < 10) { $0(); }
  }
};
const test2 = cond => {
  const $0 = Effect$dConsole.log("foo");
  return () => {
    while (cond.value) {
      $0();
    }
    const $1 = Effect$dConsole.log("bar");
    while (cond.value) {
      $1();
    }
  };
};
const test1 = cond => {
  const $0 = Effect$dConsole.log("foo");
  return () => {
    while (cond.value) {
      $0();
      Effect$dConsole.log("bar")();
    }
  };
};
export {test1, test2, test3, test4};

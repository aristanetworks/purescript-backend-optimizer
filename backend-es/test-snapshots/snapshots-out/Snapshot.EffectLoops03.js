import * as $mEffect$dConsole from "../Effect.Console/index.js";
const test4 = cond => ref => () => {
  while (cond.value) {
    const a = ref.value;
    if (a < 10) {
      $mEffect$dConsole.log("foo")();
    } else {
      $mEffect$dConsole.log("wat")();
    }
  }
};
const test3 = cond => ref => () => {
  while (cond.value) {
    const a = ref.value;
    const $0 = $mEffect$dConsole.log("foo");
    if (a < 10) { $0(); }
  }
};
const test2 = cond => {
  const $0 = $mEffect$dConsole.log("foo");
  return () => {
    while (cond.value) {
      $0();
    }
    const $1 = $mEffect$dConsole.log("bar");
    while (cond.value) {
      $1();
    }
  };
};
const test1 = cond => {
  const $0 = $mEffect$dConsole.log("foo");
  return () => {
    while (cond.value) {
      $0();
      $mEffect$dConsole.log("bar")();
    }
  };
};
export {test1, test2, test3, test4};

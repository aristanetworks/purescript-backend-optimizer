import * as $mEffect$dConsole from "../Effect.Console/index.js";
const test1 = /* #__PURE__ */ (() => {
  const $0 = $mEffect$dConsole.log("1");
  return () => {
    $0();
    const value = $mEffect$dConsole.log("2")();
    $mEffect$dConsole.log("3")();
    return value;
  };
})();
export {test1};

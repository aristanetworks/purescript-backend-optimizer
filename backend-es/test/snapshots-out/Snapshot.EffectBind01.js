import * as Effect$dConsole from "../Effect.Console/index.js";
const test1 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("1");
  return () => {
    $0();
    const value = Effect$dConsole.log("2")();
    Effect$dConsole.log("3")();
    return value;
  };
})();
export {test1};

import * as Effect$dConsole from "../Effect.Console/index.js";
const test1$0 = /* #__PURE__ */ Effect$dConsole.log("1");
const test1 = () => {
  test1$0();
  Effect$dConsole.log("2")();
  Effect$dConsole.log("3")();
  return value$1;
};
export {test1, test1$0};

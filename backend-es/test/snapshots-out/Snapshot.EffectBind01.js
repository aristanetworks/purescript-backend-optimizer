import * as Effect$dConsole from "../Effect.Console/index.js";
const test1$d0 = /* #__PURE__ */ Effect$dConsole.log("1");
const test1 = () => {
  test1$d0();
  const value = Effect$dConsole.log("2")();
  Effect$dConsole.log("3")();
  return value;
};
export {test1, test1$d0};

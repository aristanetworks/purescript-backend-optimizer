import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test2 = random => () => {
  const n = random();
  return Effect$dConsole.log(Data$dShow.showIntImpl(n))();
};
const test1 = /* #__PURE__ */ Effect$dConsole.log("12");
export {test1, test2};

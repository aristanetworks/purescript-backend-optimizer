import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test1 = /* #__PURE__ */ (() => {
  const b = Effect$dConsole.log("foo");
  return Data$dTuple.$Tuple(5, Data$dTuple.$Tuple(Data$dTuple.$Tuple(1, Data$dTuple.$Tuple(b, Data$dTuple.$Tuple(5, b))), 6));
})();
export {test1};

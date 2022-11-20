import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test = k => () => {
  for (const a of k(42)) {
    Effect$dConsole.log(Data$dShow.showIntImpl(a))();
    Effect$dConsole.log(Data$dShow.showIntImpl(a))();
  }
};
export {test};

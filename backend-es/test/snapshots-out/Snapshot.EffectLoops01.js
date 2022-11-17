import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test = () => {
  for (const a of [1, 2, 3]) {
    const $1 = Effect$dConsole.log(Data$dShow.showIntImpl(a));
    $1();
    Effect$dConsole.log(Data$dShow.showIntImpl(a))();
  }
};
export {test};

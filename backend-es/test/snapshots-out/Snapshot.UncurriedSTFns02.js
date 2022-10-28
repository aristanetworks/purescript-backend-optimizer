import * as $runtime from "../runtime.js";
import * as Control$dMonad$dST$dInternal from "../Control.Monad.ST.Internal/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import {random} from "./foreign.js";
const test2 = () => {
  const n = random();
  return Effect$dConsole.log(Data$dShow.showIntImpl(Control$dMonad$dST$dInternal.run(() => n)))();
};
const test1 = () => 12;
export {test1, test2};
export * from "./foreign.js";

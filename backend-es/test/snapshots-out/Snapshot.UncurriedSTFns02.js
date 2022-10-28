import * as $runtime from "../runtime.js";
import * as Control$dMonad$dST$dUncurried from "../Control.Monad.ST.Uncurried/index.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import {random} from "./foreign.js";
const swallow = dictApplicative => v => dictApplicative.pure(Data$dUnit.unit);
const test1 = /* #__PURE__ */ () => /* #__PURE__ */ Control$dMonad$dST$dUncurried.mkSTFn1(m => () => Data$dUnit.unit)(12);
const test2 = () => {
  random();
  return Effect$dConsole.log("unit")();
};
export {swallow, test1, test2};
export * from "./foreign.js";

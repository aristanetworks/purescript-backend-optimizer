import * as $runtime from "../runtime.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const test1 = () => {
  Effect$dConsole.log("foo")();
  const a = Effect$dRandom.randomInt(1)(10)();
  const a$1 = Effect$dRandom.randomInt(1)(10)();
  const a$p = (() => {
    const $3 = a$1 + 1 | 0;
    return () => {
      const a$2 = Effect$dRandom.randomInt(1)(10)();
      const a$3 = Effect$dRandom.randomInt(1)(10)();
      const a$p = (() => {
        const $6 = ((4 + a | 0) + $3 | 0) + (a$2 + a$3 | 0) | 0;
        return () => Data$dTuple.$Tuple($6, "");
      })()();
      return Data$dTuple.$Tuple(a$p._1, "nothing" + a$p._2);
    };
  })()();
  return a$p._1;
};
export {test1};

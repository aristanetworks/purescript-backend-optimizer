import * as $runtime from "../runtime.js";
import * as Data$dRing from "../Data.Ring/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Effect from "../Effect/index.js";
import * as Snapshot$dEffectBind06$foreign from "./foreign.js";
const random = Snapshot$dEffectBind06$foreign.random;
const test = () => {
  const x = random();
  const x1 = random();
  const y = random();
  const m = random();
  return (x + (x1 + y | 0) | 0) - m | 0;
};
export {random, test};
export * from "./foreign.js";

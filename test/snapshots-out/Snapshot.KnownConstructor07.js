import * as $runtime from "../runtime.js";
import * as Data$dRing from "../Data.Ring/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Snapshot$dKnownConstructor07$foreign from "./foreign.js";
const f = Snapshot$dKnownConstructor07$foreign.f;
const test = y => {
  const z = f(y);
  return {bar: z - 2 | 0, foo: z + 1 | 0};
};
export {f, test};
export * from "./foreign.js";

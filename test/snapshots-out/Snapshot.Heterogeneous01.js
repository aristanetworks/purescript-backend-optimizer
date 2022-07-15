import * as Control$dSemigroupoid from "./Control.Semigroupoid.js";
import * as Data$dHeytingAlgebra from "./Data.HeytingAlgebra.js";
import * as Data$dSemiring from "./Data.Semiring.js";
import * as Data$dTuple from "./Data.Tuple.js";
import * as Record$dUnsafe from "./Record.Unsafe.js";
const test2 = (() => {
  const _0 = Data$dTuple.Tuple("bar");
  return r1 => ({...r1, a: 1 + r1.a | 0, b: _0(r1.b), c: !r1.c});
})();
const test1 = (() => {
  const _0 = {a: 12, b: 42.0, c: true};
  return {..._0, a: 1 + _0.a | 0, b: Data$dTuple.$Tuple("bar", _0.b), c: !_0.c};
})();
export {test1, test2};
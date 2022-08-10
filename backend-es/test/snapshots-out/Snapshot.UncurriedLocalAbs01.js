import * as $runtime from "../runtime.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
const sum = /* #__PURE__ */ Data$dFoldable.foldlArray(Data$dSemiring.intAdd)(0);
const test = x => y => {
  const fn = (a, b) => sum([x, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b]);
  return fn(x, y) + fn(y, x) | 0;
};
export {sum, test};

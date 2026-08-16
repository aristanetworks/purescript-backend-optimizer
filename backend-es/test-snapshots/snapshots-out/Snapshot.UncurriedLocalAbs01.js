import * as $mData$dFoldable from "../Data.Foldable/index.js";
import * as $mData$dSemiring from "../Data.Semiring/index.js";
const sum = /* #__PURE__ */ $mData$dFoldable.foldlArray($mData$dSemiring.intAdd)(0);
const test = x => y => {
  const fn = (a, b) => sum([x, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b]);
  return fn(x, y) + fn(y, x) | 0;
};
export {sum, test};

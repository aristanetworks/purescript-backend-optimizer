import * as $runtime from "../runtime.js";
import * as Control$dSemigroupoid from "../Control.Semigroupoid/index.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Snapshot$dFunctionCompose03$foreign from "./foreign.js";
const f = Snapshot$dFunctionCompose03$foreign.f;
const g = Snapshot$dFunctionCompose03$foreign.g;
const test4 = /* #__PURE__ */ (() => {
  const _0 = g(Data$dUnit.unit);
  const _1 = f(Data$dUnit.unit);
  const _2 = g(Data$dUnit.unit);
  const _3 = f(Data$dUnit.unit);
  const _4 = g(Data$dUnit.unit);
  return x => _0(_1(_2(_3(_4(x)))));
})();
const test3 = /* #__PURE__ */ (() => {
  const _0 = f(Data$dUnit.unit);
  const _1 = g(Data$dUnit.unit);
  const _2 = f(Data$dUnit.unit);
  const _3 = g(Data$dUnit.unit);
  return x => _0(_1(_2(_3(x))));
})();
const test2 = /* #__PURE__ */ (() => {
  const _0 = g(Data$dUnit.unit);
  const _1 = f(Data$dUnit.unit);
  const _2 = g(Data$dUnit.unit);
  return x => _0(_1(_2(x)));
})();
const test1 = /* #__PURE__ */ (() => {
  const _0 = f(Data$dUnit.unit);
  const _1 = g(Data$dUnit.unit);
  return x => _0(_1(x));
})();
export {f, g, test1, test2, test3, test4};
export * from "./foreign.js";

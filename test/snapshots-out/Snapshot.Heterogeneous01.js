// @inline Heterogeneous.Mapping.hmapRecord arity=2
// @inline Heterogeneous.Mapping.hmapWithIndexRecord arity=2
// @inline Heterogeneous.Mapping.mapRecordWithIndexCons arity=5
// @inline Heterogeneous.Mapping.mapRecordWithIndexNil.mapRecordWithIndexBuilder arity=2
import * as $runtime from "../runtime.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const test2 = /* #__PURE__ */ (() => {
  const _0 = Data$dTuple.Tuple("bar");
  return r1 => ({...r1, a: 1 + r1.a | 0, b: _0(r1.b), c: !r1.c});
})();
const test1 = /* #__PURE__ */ (() => {
  const _0 = {a: 12, b: 42.0, c: true};
  return {..._0, a: 1 + _0.a | 0, b: Data$dTuple.$Tuple("bar", _0.b), c: !_0.c};
})();
export {test1, test2};

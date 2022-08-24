// @inline Heterogeneous.Mapping.hmapRecord arity=2
// @inline Heterogeneous.Mapping.hmapWithIndexRecord arity=2
// @inline Heterogeneous.Mapping.mapRecordWithIndexCons arity=5
// @inline Heterogeneous.Mapping.mapRecordWithIndexNil.mapRecordWithIndexBuilder arity=2
import * as $runtime from "../runtime.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const test2 = /* #__PURE__ */ (() => {
  const $0 = Data$dTuple.Tuple("bar");
  return r1 => ({...r1, a: 1 + r1.a | 0, b: $0(r1.b), c: !r1.c});
})();
const test1 = {a: 13, b: /* #__PURE__ */ Data$dTuple.$Tuple("bar", 42.0), c: false};
export {test1, test2};

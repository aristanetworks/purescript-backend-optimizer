import * as $runtime from "../runtime.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import * as Snapshot$dDefaultRulesMonoid02$foreign from "./foreign.js";
const x = Snapshot$dDefaultRulesMonoid02$foreign.x;
const test1 = /* #__PURE__ */ (() => Data$dSemigroup.semigroupRecordCons({reflectSymbol: () => "bar"})()(Data$dSemigroup.semigroupRecordCons({reflectSymbol: () => "foo"})()(Data$dSemigroup.semigroupRecordNil)(Data$dSemigroup.semigroupString))(Data$dSemigroup.semigroupArray).appendRecord(Type$dProxy.Proxy))();
export {test1, x};
export * from "./foreign.js";

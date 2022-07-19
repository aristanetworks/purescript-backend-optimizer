import * as $runtime from "./runtime.js";
import * as Data$dSemigroup from "./Data.Semigroup.js";
import * as Snapshot$dPrimOpString03$foreign from "./Snapshot.PrimOpString03.foreign.js";
const a = Snapshot$dPrimOpString03$foreign.a;
const test3 = /* #__PURE__ */ (() => "ab" + a + "cd")();
const test2 = /* #__PURE__ */ (() => "ab" + a + "cd")();
const test1 = /* #__PURE__ */ (() => "ab" + (a + "cd"))();
export {a, test1, test2, test3};
export * from "./Snapshot.PrimOpString03.foreign.js";

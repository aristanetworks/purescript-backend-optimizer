import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Snapshot$dDefaultRulesFunctor01$foreign from "./foreign.js";
const mb = Snapshot$dDefaultRulesFunctor01$foreign.mb;
const test5 = /* #__PURE__ */ (() => {
  const _0 = (() => {
    if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", v => mb._1); }
    return Data$dMaybe.Nothing;
  })();
  if (_0.tag === "Just") { return Data$dMaybe.$Maybe("Just", _0._1(12)); }
  return Data$dMaybe.Nothing;
})();
const test4 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", 42); }
  return Data$dMaybe.Nothing;
})();
const test3 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", 42); }
  return Data$dMaybe.Nothing;
})();
const test2 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dUnit.unit); }
  return Data$dMaybe.Nothing;
})();
const test1 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dShow.showIntImpl(mb._1)); }
  return Data$dMaybe.Nothing;
})();
export {mb, test1, test2, test3, test4, test5};
export * from "./foreign.js";

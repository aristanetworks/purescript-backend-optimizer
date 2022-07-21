import * as $runtime from "../runtime.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Snapshot$dDefaultRulesMonoid01$foreign from "./foreign.js";
const f = Snapshot$dDefaultRulesMonoid01$foreign.f;
const test2 = /* #__PURE__ */ (() => {
  const _0 = f([1, 2, 3]);
  return a => {
    if (a) { return _0; }
    return [];
  };
})();
const test1 = a => {
  if (a) { return [1, 2, 3]; }
  return [];
};
export {f, test1, test2};
export * from "./foreign.js";

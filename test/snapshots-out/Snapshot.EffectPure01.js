import * as $runtime from "../runtime.js";
import * as Effect from "../Effect/index.js";
import * as Snapshot$dEffectPure01$foreign from "./foreign.js";
const a = Snapshot$dEffectPure01$foreign.a;
const test2 = /* #__PURE__ */ (() => {
  const _0 = a + 1 | 0;
  return () => _0;
})();
const test1 = () => 1;
export {a, test1, test2};
export * from "./foreign.js";

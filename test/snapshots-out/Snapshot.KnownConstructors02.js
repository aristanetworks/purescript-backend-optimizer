import * as $runtime from "./runtime.js";
import * as Snapshot$dKnownConstructors02$foreign from "./Snapshot.KnownConstructors02.foreign.js";
const a = Snapshot$dKnownConstructors02$foreign.a;
const test = /* #__PURE__ */ (() => {
  if (a.tag === "Left") { return a._1; }
  if (a.tag === "Right") { return a._1; }
  throw new Error("Failed pattern match");
})();
export {a, test};
export * from "./Snapshot.KnownConstructors02.foreign.js";

import * as $runtime from "../runtime.js";
import {a} from "./foreign.js";
const test = /* #__PURE__ */ (() => {
  if (a.tag === "Left") { return a._1; }
  if (a.tag === "Right") { return a._1; }
  $runtime.fail();
})();
export {test};
export * from "./foreign.js";

import * as $runtime from "../runtime.js";
import {a} from "./foreign.js";
const test3 = /* #__PURE__ */ (() => "ab" + a + "cd")();
const test2 = /* #__PURE__ */ (() => "ab" + a + "cd")();
const test1 = /* #__PURE__ */ (() => "ab" + (a + "cd"))();
export {test1, test2, test3};
export * from "./foreign.js";

import * as $runtime from "../runtime.js";
import {f} from "./foreign.js";
const test3 = x => {
  if (x > 42) { return false; }
  $runtime.fail();
};
const test2 = x => {
  if (x > 42) { return f("Hello, World")("Hello, Universe"); }
  $runtime.fail();
};
const test1 = x => {
  if (x > 42) { return ["Hello, World", "Hello, Universe"]; }
  $runtime.fail();
};
export {test1, test2, test3};
export * from "./foreign.js";

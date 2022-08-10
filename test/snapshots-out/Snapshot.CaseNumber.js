import * as $runtime from "../runtime.js";
const test1 = v => {
  if (v === 1.0) { return "1"; }
  if (v === 2.0) { return "2"; }
  if (v === 3.0) { return "3"; }
  return "catch";
};
export {test1};

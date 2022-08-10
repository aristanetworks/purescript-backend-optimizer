import * as $runtime from "../runtime.js";
const NewTypeInt = x => x;
const test1 = v => {
  if (v === 1) { return "1"; }
  if (v === 2) { return "2"; }
  if (v === 3) { return "3"; }
  return "catch";
};
export {NewTypeInt, test1};

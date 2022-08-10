import * as $runtime from "../runtime.js";
const test2 = v => {
  if (v.a.c === 2) {
    if (v.d.e === 1) {
      if (v.d.f === 2) {
        if (v.a.b === 1) { return 1; }
        return 2;
      }
      if (v.a.b === 1) { return 3; }
      return 4;
    }
    if (v.a.b === 1) { return 3; }
    return 4;
  }
  return 4;
};
const test1 = v => {
  if (v.a === 1) { return "0"; }
  return "catch";
};
export {test1, test2};

import * as $runtime from "../runtime.js";
const test1 = v => v1 => v2 => {
  if (!v1) {
    if (v2) { return 1; }
    return 3;
  }
  if (!v) {
    if (v1) { return 2; }
    if (!v2) { return 3; }
    if (v2) { return 4; }
    $runtime.fail();
  }
  if (!v2) { return 3; }
  if (v2) { return 4; }
  $runtime.fail();
};
export {test1};

import * as $runtime from "../runtime.js";
import {f} from "./foreign.js";
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
export {test1, test2};
export * from "./foreign.js";

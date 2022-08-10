// @inline Data.Maybe.maybe arity=2
// @inline Data.Maybe.maybe' arity=2
import * as $runtime from "../runtime.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import {a, f, g} from "./foreign.js";
const test5 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return v2 => {
    if (v2.tag === "Nothing") { return a + 1 | 0; }
    if (v2.tag === "Just") { return _0(v2._1); }
    $runtime.fail();
  };
})();
const test4 = /* #__PURE__ */ (() => {
  const _0 = g(1);
  return v2 => {
    if (v2.tag === "Nothing") { return f(Data$dUnit.unit); }
    if (v2.tag === "Just") { return _0(v2._1); }
    $runtime.fail();
  };
})();
const test3 = v2 => {
  if (v2.tag === "Nothing") { return f(Data$dUnit.unit); }
  if (v2.tag === "Just") { return 1 + v2._1 | 0; }
  $runtime.fail();
};
const test2 = /* #__PURE__ */ (() => {
  const _0 = f(Data$dUnit.unit);
  const _1 = g(1);
  return v2 => {
    if (v2.tag === "Nothing") { return _0; }
    if (v2.tag === "Just") { return _1(v2._1); }
    $runtime.fail();
  };
})();
const test1 = /* #__PURE__ */ (() => {
  const _0 = f(Data$dUnit.unit);
  return v2 => {
    if (v2.tag === "Nothing") { return _0; }
    if (v2.tag === "Just") { return 1 + v2._1 | 0; }
    $runtime.fail();
  };
})();
export {test1, test2, test3, test4, test5};
export * from "./foreign.js";

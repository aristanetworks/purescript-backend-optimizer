import * as $runtime from "../runtime.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import {x} from "./foreign.js";
const eq = ra => rb => ra.bar === rb.bar && ra.baz === rb.baz && ra.foo === rb.foo;
const test9 = /* #__PURE__ */ (() => "hello" === x(Data$dUnit.unit))();
const test8 = false;
const test7 = false;
const test6 = true;
const test5 = a => a.bar === "hello" && !a.baz && a.foo === 42;
const test4 = a => "hello" === a.bar && !a.baz && 42 === a.foo;
const test3 = rb => "hello" === rb.bar && !rb.baz && 42 === rb.foo;
const test2 = a => b => a.bar === b.bar && a.baz === b.baz && a.foo === b.foo;
const test10 = /* #__PURE__ */ (() => {
  const $0 = x(Data$dUnit.unit);
  return rb => $0 === rb.bar && rb.baz && 42 === rb.foo;
})();
const test1 = eq;
export {eq, test1, test10, test2, test3, test4, test5, test6, test7, test8, test9};
export * from "./foreign.js";

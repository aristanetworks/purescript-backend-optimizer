// @fails Binding demanded before initialized
import * as $runtime from "../runtime.js";
const test$dtest3 = n => {
  if (n < 100) { return n; }
  return test$dtest1$lazy().bar;
};
const test$dtest2$lazy = /* #__PURE__ */ $runtime.binding(() => ({baz: test$dtest1$lazy().bar}));
const test$dtest1$lazy = /* #__PURE__ */ $runtime.binding(() => ({foo: test$dtest2$lazy().baz, bar: test$dtest3(42)}));
const test$dtest2 = /* #__PURE__ */ test$dtest2$lazy();
const test$dtest1 = /* #__PURE__ */ test$dtest1$lazy();
const test = /* #__PURE__ */ (() => test$dtest1.bar)();
export {test, test$dtest1, test$dtest2, test$dtest3};

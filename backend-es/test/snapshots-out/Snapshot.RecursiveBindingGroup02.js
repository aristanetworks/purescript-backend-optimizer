// @fails Binding demanded before initialized
import * as $runtime from "../runtime.js";
const test$0$test3$$rec = n => {
  if (n < 100) { return n; }
  return test$0$test1$$rec$lazy().bar;
};
const test$0$test2$$rec$lazy = /* #__PURE__ */ $runtime.binding(() => ({baz: test$0$test1$$rec$lazy().bar}));
const test$0$test1$$rec$lazy = /* #__PURE__ */ $runtime.binding(() => ({foo: test$0$test2$$rec$lazy().baz, bar: test$0$test3$$rec(42)}));
const test$0$test2$$rec = /* #__PURE__ */ test$0$test2$$rec$lazy();
const test$0$test1$$rec = /* #__PURE__ */ test$0$test1$$rec$lazy();
const test = /* #__PURE__ */ (() => test$0$test1$$rec.bar)();
export {test, test$0$test1$$rec, test$0$test2$$rec, test$0$test3$$rec};

// @fails Binding demanded before initialized
const test$0$test3 = n => {
  if (n < 100) { return n; }
  return test$0$test1.bar;
};
const test$0$test2 = /* #__PURE__ */ (() => ({baz: test$0$test1.bar}))();
const test$0$test1 = /* #__PURE__ */ (() => ({foo: test$0$test2.baz, bar: test$0$test3(42)}))();
const test = /* #__PURE__ */ (() => test$0$test1.bar)();
export {test, test$0$test1, test$0$test2, test$0$test3};

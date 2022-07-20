import * as $runtime from "../runtime.js";
const test = /* #__PURE__ */ (() => {
  const test3 = n => {
    if (n < 100) { return n; }
    return test1$lazy().bar;
  };
  const test2$lazy = $runtime.binding(() => ({baz: test1$lazy().bar}));
  const test1$lazy = $runtime.binding(() => ({foo: test2$lazy().baz, bar: test3(42)}));
  const test2 = test2$lazy();
  const test1 = test1$lazy();
  return test1.bar;
})();
export {test};

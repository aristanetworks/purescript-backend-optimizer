import * as $runtime from "../runtime.js";
const test$0$k$$rec = m => {
  if (m === 100) { return test$0$go$$rec$lazy()(m - 1 | 0); }
  if (m === 900) { return 42; }
  return test$0$k$$rec(m - 1 | 0);
};
const test = n => {
  if (n === 0) { return n; }
  if (n <= 100) { return test$0$go$$rec$lazy()(n - 1 | 0); }
  return test$0$k$$rec(n - 1 | 0);
};
const test$0$go$$rec$lazy = /* #__PURE__ */ $runtime.binding(() => test);
const test$0$go$$rec = /* #__PURE__ */ test$0$go$$rec$lazy();
export {test, test$0$go$$rec, test$0$k$$rec};

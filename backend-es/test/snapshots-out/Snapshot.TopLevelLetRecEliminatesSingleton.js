const test$0$k$$rec = m => {
  if (m === 100) { return test(m - 1 | 0); }
  if (m === 900) { return 42; }
  return test$0$k$$rec(m - 1 | 0);
};
const test = n => {
  if (n === 0) { return n; }
  if (n <= 100) { return test(n - 1 | 0); }
  return test$0$k$$rec(n - 1 | 0);
};
export {test, test$0$k$$rec};

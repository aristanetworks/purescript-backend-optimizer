import * as Control$dMonad$dST$dUncurried from "../Control.Monad.ST.Uncurried/index.js";
const test6 = f => g => {
  const $0 = g(1);
  return () => {
    f($0, 2, 3);
    f(g(1), 2, 3);
    return f(g(1), 2, 3);
  };
};
const test5 = f => g => {
  const $0 = g(1);
  return () => {
    f($0, 2, 3);
    return f(g(1), 2, 3);
  };
};
const test4 = f => Control$dMonad$dST$dUncurried.runSTFn3(f);
const test3 = f => g => Control$dMonad$dST$dUncurried.runSTFn3(f)(g(1));
const test2 = f => g => Control$dMonad$dST$dUncurried.runSTFn3(f)(g(1))(2);
const test1 = f => g => {
  const $0 = g(1);
  return () => f($0, 2, 3);
};
export {test1, test2, test3, test4, test5, test6};

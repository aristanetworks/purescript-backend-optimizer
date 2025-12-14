const test4 = f => $0 => $1 => $2 => f($0, $1, $2);
const test3 = f => g => {
  const $0 = g(1);
  return $1 => $2 => f($0, $1, $2);
};
const test2 = f => g => {
  const $0 = g(1);
  return $1 => f($0, 2, $1);
};
const test1 = f => g => f(g(1), 2, 3);
export {test1, test2, test3, test4};

const test4 = f => g => {
  const $0 = g();
  const $1 = f();
  const $2 = f();
  const $3 = g();
  const $4 = g();
  return x => $0($2($3($1($4(x)))));
};
const test3 = f => g => {
  const $0 = f();
  const $1 = f();
  const $2 = g();
  const $3 = g();
  return x => $0($2($1($3(x))));
};
const test2 = f => g => {
  const $0 = f();
  const $1 = g();
  const $2 = g();
  return x => $1($0($2(x)));
};
const test1 = f => g => {
  const $0 = f();
  const $1 = g();
  return x => $0($1(x));
};
export {test1, test2, test3, test4};

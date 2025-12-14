// @inline export fn never
// @inline export localTest always
// @inline export externTest always
const fn = v => 0;
const localTest = f => {
  const rec = {a: {b: {c: 99}}, d: fn({}), e: 11};
  const res = f(rec);
  if (res !== -2147483648) { return res; }
  return fn(rec);
};
const test1 = 110;
const test2 = 88;
const test3 = 1089;
const test4 = 9;
const extern = {a: {b: {c: 99}}, d: /* #__PURE__ */ fn({}), e: 11};
const externTest = f => {
  const res = f(extern);
  if (res !== -2147483648) { return res; }
  return -2147483648;
};
const test5 = 110;
const test6 = 88;
const test7 = 1089;
const test8 = 9;
export {extern, externTest, fn, localTest, test1, test2, test3, test4, test5, test6, test7, test8};

// @inline export fn never
// @inline export localTest always
// @inline export externTest always
const fn = v => 0.0;
const localTest = f => {
  const rec = {a: {b: {c: 99.0}}, d: fn({}), e: 11.0};
  const res = f(rec);
  if (res !== -Infinity) { return res; }
  return fn(rec);
};
const test1 = 110.0;
const test2 = 88.0;
const test3 = 1089.0;
const test4 = 9.0;
const extern = {a: {b: {c: 99.0}}, d: /* #__PURE__ */ fn({}), e: 11.0};
const externTest = f => {
  const res = f(extern);
  if (res !== -Infinity) { return res; }
  return -Infinity;
};
const test5 = 110.0;
const test6 = 88.0;
const test7 = 1089.0;
const test8 = 9.0;
export {extern, externTest, fn, localTest, test1, test2, test3, test4, test5, test6, test7, test8};

// @inline export fn never
const fn = v => 0;
const test1 = 42;
const test2 = 42;
const test3 = 42;
const extern1 = {a: {b: {c: true}}, d: /* #__PURE__ */ fn({}), e: true, f: false};
const test4 = 42;
const test5 = 42;
const test6 = 42;
export {extern1, fn, test1, test2, test3, test4, test5, test6};

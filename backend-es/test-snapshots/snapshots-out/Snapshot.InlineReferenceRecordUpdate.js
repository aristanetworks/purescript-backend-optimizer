// @inline export fn' never
const test1 = fn => fn({}).c;
const fn$p = v => ({a: 1, b: 2, c: 3});
const extern = {.../* #__PURE__ */ fn$p({}), a: 42};
const test2 = /* #__PURE__ */ (() => extern.c)();
export {extern, fn$p, test1, test2};

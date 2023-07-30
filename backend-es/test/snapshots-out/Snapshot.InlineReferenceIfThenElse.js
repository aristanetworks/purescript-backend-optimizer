// @inline export fn never
const fn = v => 0;
const test1 = 42;
const extern1 = {a: {b: {c: true}}, d: /* #__PURE__ */ fn({})};
const test2 = 42;
export {extern1, fn, test1, test2};

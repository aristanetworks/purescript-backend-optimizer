// @inline export fn' never
const test2 = fn => [[1, 2, fn({})], [3, 4], [fn({})]];
const test1 = fn => [1, 2, fn({})];
const fn$p = v => 0;
const extern1 = [1, 2, /* #__PURE__ */ fn$p({})];
const extern2 = [extern1, [3], [/* #__PURE__ */ fn$p({})]];
const test4 = extern2;
const test3 = extern1;
export {extern1, extern2, fn$p, test1, test2, test3, test4};

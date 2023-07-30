// @inline export fn' never
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Cons = value0 => value1 => $List("Cons", value0, value1);
const Nil = /* #__PURE__ */ $List("Nil");
const test3 = fn => $List("Cons", 0, $List("Cons", 1, fn({})));
const test2 = fn => $List("Cons", 0, $List("Cons", 1, fn({})));
const test1 = fn => $List("Cons", 0, $List("Cons", 1, fn({})));
const fn$p = v => Nil;
const extern2 = {a: {b: {c: /* #__PURE__ */ $List("Cons", 1, /* #__PURE__ */ fn$p({}))}}};
const extern3 = {d: extern2, e: /* #__PURE__ */ fn$p({})};
const test6 = /* #__PURE__ */ (() => $List("Cons", 0, extern2.a.b.c))();
const test5 = /* #__PURE__ */ (() => $List("Cons", 0, extern2.a.b.c))();
const extern1 = /* #__PURE__ */ $List("Cons", 1, /* #__PURE__ */ fn$p({}));
const test4 = /* #__PURE__ */ $List("Cons", 0, extern1);
export {$List, Cons, Nil, extern1, extern2, extern3, fn$p, test1, test2, test3, test4, test5, test6};

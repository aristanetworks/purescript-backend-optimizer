// @inline Snapshot.RecursionInlined02.addStuff always
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const addStuff = x => ys => {
  if (x > 0) { return 1 + addStuff(x - 1 | 0)(ys) | 0; }
  if (x < 0) { return -1 + addStuff(x + 1 | 0)(ys) | 0; }
  return ys;
};
const test1 = /* #__PURE__ */ (() => 38 + 4 | 0)();
const test2 = z => 3 + addStuff(z)(5) | 0;
export {$List, Cons, Nil, addStuff, test1, test2};

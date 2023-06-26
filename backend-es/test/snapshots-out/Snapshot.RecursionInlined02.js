// @inline Snapshot.RecursionInlined02.addStuff always
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const addStuff = x => ys => {
  if (x > 0) { return 1 + addStuff(x - 1 | 0)(ys) | 0; }
  if (x < 0) { return -1 + addStuff(x + 1 | 0)(ys) | 0; }
  return ys;
};
const test1 = 42;
const test2 = z => 2 + addStuff(1)((() => {
  if (z > 0) {
    const $0 = z - 1 | 0;
    if ($0 > 0) { return 2 + addStuff($0 - 1 | 0)(5) | 0; }
    if ($0 < 0) { return 0 + addStuff($0 + 1 | 0)(5) | 0; }
    return 6;
  }
  if (z < 0) {
    const $0 = z + 1 | 0;
    if ($0 > 0) { return 0 + addStuff($0 - 1 | 0)(5) | 0; }
    if ($0 < 0) { return -2 + addStuff($0 + 1 | 0)(5) | 0; }
    return 4;
  }
  return 5;
})()) | 0;
export {$List, Cons, Nil, addStuff, test1, test2};

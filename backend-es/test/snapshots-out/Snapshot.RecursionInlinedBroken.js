// @inline Snapshot.RecursionInlinedBroken.addStuffBroken always
// This will recurse out of control and stop when it hits the recursion limit
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const addStuffBroken = v => v1 => {
  if (v === 0) { return v1; }
  return 1 + addStuffBroken(v - 1 | 0)(v1) | 0;
};
const test1 = /* #__PURE__ */ (() => 2 + addStuffBroken(-5)(4) | 0)();
const test2 = z => 2 + addStuffBroken(-5)((() => {
  if (z === 0) { return 5; }
  const $0 = z - 1 | 0;
  if ($0 === 0) { return 6; }
  return 2 + addStuffBroken($0 - 1 | 0)(5) | 0;
})()) | 0;
export {$List, Cons, Nil, addStuffBroken, test1, test2};

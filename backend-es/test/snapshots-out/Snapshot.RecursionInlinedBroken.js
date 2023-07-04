// @inline Snapshot.RecursionInlinedBroken.addStuffBroken always
// This will recurse out of control and stop when it hits the recursion limit
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const addStuffBroken = v => v1 => {
  if (v === 0) { return v1; }
  return 1 + addStuffBroken(v - 1 | 0)(v1) | 0;
};
const test1 = v => 9998 + addStuffBroken(-10001)(4) | 0;
const test2 = z => 9998 + addStuffBroken(-10001)(addStuffBroken(z)(5)) | 0;
export {$List, Cons, Nil, addStuffBroken, test1, test2};

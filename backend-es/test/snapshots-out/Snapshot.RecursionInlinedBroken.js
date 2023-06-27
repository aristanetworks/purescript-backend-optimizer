// @inline Snapshot.RecursionInlinedBroken.addStuffBroken always
// This will recurse out of control and stop when it hits the recursion limit
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const addStuffBroken = v => v1 => {
  if (v === 0) { return v1; }
  return 1 + addStuffBroken(v - 1 | 0)(v1) | 0;
};
const test1 = v => addStuffBroken(-3)(4);
const test2 = z => addStuffBroken(-3)(addStuffBroken(z)(5));
export {$List, Cons, Nil, addStuffBroken, test1, test2};

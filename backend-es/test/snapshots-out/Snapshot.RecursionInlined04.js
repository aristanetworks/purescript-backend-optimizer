// @inline Snapshot.RecursionInlined04.foldlArray always
// @inline Snapshot.RecursionInlined04.foldlArray2 always
// A "real-world" example from deku
import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
const foldlArray2 = foldlArray2$a0$copy => foldlArray2$a1$copy => foldlArray2$a2$copy => foldlArray2$a3$copy => foldlArray2$a4$copy => {
  let foldlArray2$a0 = foldlArray2$a0$copy;
  let foldlArray2$a1 = foldlArray2$a1$copy;
  let foldlArray2$a2 = foldlArray2$a2$copy;
  let foldlArray2$a3 = foldlArray2$a3$copy;
  let foldlArray2$a4 = foldlArray2$a4$copy;
  let foldlArray2$c = true;
  let foldlArray2$r;
  while (foldlArray2$c) {
    const n = foldlArray2$a0, i = foldlArray2$a1, bab = foldlArray2$a2, b = foldlArray2$a3, arr = foldlArray2$a4;
    if (n === i) {
      foldlArray2$c = false;
      foldlArray2$r = b;
      continue;
    }
    foldlArray2$a0 = n + 1 | 0;
    foldlArray2$a1 = i;
    foldlArray2$a2 = bab;
    foldlArray2$a3 = bab(b)(arr[n]);
    foldlArray2$a4 = arr;
  }
  return foldlArray2$r;
};
const foldlArray = bab => b => arr => foldlArray2(0)(arr.length)(bab)(b)(arr);
const test1 = /* #__PURE__ */ foldlArray2(0)(1)(v => {
  const $0 = v.left;
  const $1 = v.right;
  return v1 => {
    if (v1.tag === "Left") { return {left: Data$dSemigroup.concatArray($0)([v1._1]), right: $1}; }
    if (v1.tag === "Right") { return {left: $0, right: Data$dSemigroup.concatArray($1)([v1._1])}; }
    $runtime.fail();
  };
})({left: [], right: []})([/* #__PURE__ */ Data$dEither.$Either("Left", "a")]);
export {foldlArray, foldlArray2, test1};

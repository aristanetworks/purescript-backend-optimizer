// @inline Snapshot.RecursionInlined06.foldlArray always
// @inline Snapshot.RecursionInlined06.foldlArray2 always
// @inline Snapshot.RecursionInlined06.actualizeNuts always
// Another "real-world" example from deku
import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const ActualizedKorok = x => x;
const PureKorok = x => x;
const Nut = x => x;
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
const actualizeNuts = count => foldlArray2(0)(1)(v => {
  const $0 = v._1;
  const $1 = v._2;
  return v1 => {
    if (v1.tag === "Left") {
      const v2 = v1._1({count: $1});
      return Data$dTuple.$Tuple(
        Data$dSemigroup.concatArray($0)([Data$dEither.$Either("Left", v2)]),
        (() => {
          if (v2.tag === "Left") { return $1; }
          if (v2.tag === "Right") { return v2._1.count; }
          $runtime.fail();
        })()
      );
    }
    if (v1.tag === "Right") { return Data$dTuple.$Tuple(Data$dSemigroup.concatArray($0)([Data$dEither.$Either("Right", v1._1)]), $1); }
    $runtime.fail();
  };
})(Data$dTuple.$Tuple([], count + 1 | 0))([Data$dEither.$Either("Left", v => Data$dEither.$Either("Right", {count: v.count}))]);
const test1 = /* #__PURE__ */ foldlArray2(0)(1)(v => {
  const $0 = v._1;
  const $1 = v._2;
  return v1 => {
    if (v1.tag === "Left") {
      const v2 = v1._1({count: $1});
      return Data$dTuple.$Tuple(
        Data$dSemigroup.concatArray($0)([Data$dEither.$Either("Left", v2)]),
        (() => {
          if (v2.tag === "Left") { return $1; }
          if (v2.tag === "Right") { return v2._1.count; }
          $runtime.fail();
        })()
      );
    }
    if (v1.tag === "Right") { return Data$dTuple.$Tuple(Data$dSemigroup.concatArray($0)([Data$dEither.$Either("Right", v1._1)]), $1); }
    $runtime.fail();
  };
})(/* #__PURE__ */ Data$dTuple.$Tuple([], 43))([/* #__PURE__ */ Data$dEither.$Either("Left", v => Data$dEither.$Either("Right", {count: v.count}))]);
export {ActualizedKorok, Nut, PureKorok, actualizeNuts, foldlArray, foldlArray2, test1};

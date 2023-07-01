// @inline Snapshot.RecursionInlined06.foldlArray always
// @inline Snapshot.RecursionInlined06.foldlArray2 always
// @inline Snapshot.RecursionInlined06.actualizeNuts always
// Another "real-world" example from deku
import * as Data$dEither from "../Data.Either/index.js";
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
const actualizeNuts = count => {
  const $0 = count + 1 | 0;
  return Data$dTuple.$Tuple([Data$dEither.$Either("Left", Data$dEither.$Either("Right", {count: $0}))], $0);
};
const test1 = /* #__PURE__ */ Data$dTuple.$Tuple([/* #__PURE__ */ Data$dEither.$Either("Left", /* #__PURE__ */ Data$dEither.$Either("Right", {count: 43}))], 43);
export {ActualizedKorok, Nut, PureKorok, actualizeNuts, foldlArray, foldlArray2, test1};

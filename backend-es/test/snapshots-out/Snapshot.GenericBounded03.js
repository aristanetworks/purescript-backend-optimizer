// @inline export genericTest3.to arity=1
import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Snapshot$dGenericBounded01 from "../Snapshot.GenericBounded01/index.js";
import * as Snapshot$dGenericBounded02 from "../Snapshot.GenericBounded02/index.js";
const $Test3 = (_1, _2, _3, _4, _5) => ({tag: "Both3", _1, _2, _3, _4, _5});
const Both3 = value0 => value1 => value2 => value3 => value4 => $Test3(value0, value1, value2, value3, value4);
const eqTest3 = {
  eq: x => y => x._1 === y._1 && x._2 === y._2 && (() => {
    if (x._3.tag === "Bottom1") { return y._3.tag === "Bottom1"; }
    if (x._3.tag === "Ignored1") { return y._3.tag === "Ignored1"; }
    if (x._3.tag === "Top1") { return y._3.tag === "Top1"; }
    return false;
  })() && Snapshot$dGenericBounded02.eqTest2.eq(x._4)(y._4) && (
    x._5.a === y._5.a && x._5.b === y._5.b && (() => {
      if (x._5.c.tag === "Bottom1") { return y._5.c.tag === "Bottom1"; }
      if (x._5.c.tag === "Ignored1") { return y._5.c.tag === "Ignored1"; }
      if (x._5.c.tag === "Top1") { return y._5.c.tag === "Top1"; }
      return false;
    })() && Snapshot$dGenericBounded02.eqTest2.eq(x._5.d)(y._5.d)
  )
};
const ordTest3 = {
  compare: x => y => {
    const v = Data$dOrd.ordInt.compare(x._1)(y._1);
    if (v.tag === "LT") { return Data$dOrdering.LT; }
    if (v.tag === "GT") { return Data$dOrdering.GT; }
    const v1 = Data$dOrd.ordBoolean.compare(x._2)(y._2);
    if (v1.tag === "LT") { return Data$dOrdering.LT; }
    if (v1.tag === "GT") { return Data$dOrdering.GT; }
    const v2 = Snapshot$dGenericBounded01.ordTest1.compare(x._3)(y._3);
    if (v2.tag === "LT") { return Data$dOrdering.LT; }
    if (v2.tag === "GT") { return Data$dOrdering.GT; }
    const v3 = Snapshot$dGenericBounded02.ordTest2.compare(x._4)(y._4);
    if (v3.tag === "LT") { return Data$dOrdering.LT; }
    if (v3.tag === "GT") { return Data$dOrdering.GT; }
    const v4 = Data$dOrd.ordInt.compare(x._5.a)(y._5.a);
    if (v4.tag === "LT") { return Data$dOrdering.LT; }
    if (v4.tag === "GT") { return Data$dOrdering.GT; }
    const v5 = Data$dOrd.ordBoolean.compare(x._5.b)(y._5.b);
    if (v5.tag === "LT") { return Data$dOrdering.LT; }
    if (v5.tag === "GT") { return Data$dOrdering.GT; }
    const v6 = Snapshot$dGenericBounded01.ordTest1.compare(x._5.c)(y._5.c);
    if (v6.tag === "LT") { return Data$dOrdering.LT; }
    if (v6.tag === "GT") { return Data$dOrdering.GT; }
    return Snapshot$dGenericBounded02.ordTest2.compare(x._5.d)(y._5.d);
  },
  Eq0: () => eqTest3
};
const genericTest3 = {
  to: x => $Test3(x._1, x._2._1, x._2._2._1, x._2._2._2._1, x._2._2._2._2),
  from: x => Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, Data$dGeneric$dRep.$Product(x._3, Data$dGeneric$dRep.$Product(x._4, x._5))))
};
const boundedTest3 = /* #__PURE__ */ (() => (
  {
    bottom: $Test3(
      -2147483648,
      false,
      Snapshot$dGenericBounded01.Bottom1,
      Snapshot$dGenericBounded02.boundedTest2.bottom,
      {a: -2147483648, b: false, c: Snapshot$dGenericBounded01.Bottom1, d: Snapshot$dGenericBounded02.boundedTest2.bottom}
    ),
    top: $Test3(
      2147483647,
      true,
      Snapshot$dGenericBounded01.Top1,
      Snapshot$dGenericBounded02.boundedTest2.top,
      {a: 2147483647, b: true, c: Snapshot$dGenericBounded01.Top1, d: Snapshot$dGenericBounded02.boundedTest2.top}
    ),
    Ord0: () => ordTest3
  }
))();
export {$Test3, Both3, boundedTest3, eqTest3, genericTest3, ordTest3};

// @inline export genericTest2.to arity=1
import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Test2 = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const Bottom2 = value0 => value1 => value2 => $Test2("Bottom2", value0, value1, value2);
const Ignored2 = value0 => value1 => value2 => $Test2("Ignored2", value0, value1, value2);
const Top2 = value0 => value1 => value2 => $Test2("Top2", value0, value1, value2);
const eqTest2 = {
  eq: x => y => {
    if (x.tag === "Bottom2") {
      if (y.tag === "Bottom2") { return x._1 === y._1 && x._2 === y._2 && (x._3.a === y._3.a && x._3.b === y._3.b); }
      return false;
    }
    if (x.tag === "Ignored2") {
      if (y.tag === "Ignored2") { return x._1 === y._1 && x._2 === y._2 && (x._3.a === y._3.a && x._3.b === y._3.b); }
      return false;
    }
    if (x.tag === "Top2") {
      if (y.tag === "Top2") { return x._1 === y._1 && x._2 === y._2 && (x._3.a === y._3.a && x._3.b === y._3.b); }
      return false;
    }
    return false;
  }
};
const ordTest2 = {
  compare: x => y => {
    if (x.tag === "Bottom2") {
      if (y.tag === "Bottom2") {
        const v = Data$dOrd.ordInt.compare(x._1)(y._1);
        if (v.tag === "LT") { return Data$dOrdering.LT; }
        if (v.tag === "GT") { return Data$dOrdering.GT; }
        const v1 = Data$dOrd.ordBoolean.compare(x._2)(y._2);
        if (v1.tag === "LT") { return Data$dOrdering.LT; }
        if (v1.tag === "GT") { return Data$dOrdering.GT; }
        const v2 = Data$dOrd.ordInt.compare(x._3.a)(y._3.a);
        if (v2.tag === "LT") { return Data$dOrdering.LT; }
        if (v2.tag === "GT") { return Data$dOrdering.GT; }
        return Data$dOrd.ordBoolean.compare(x._3.b)(y._3.b);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Bottom2") { return Data$dOrdering.GT; }
    if (x.tag === "Ignored2") {
      if (y.tag === "Ignored2") {
        const v = Data$dOrd.ordInt.compare(x._1)(y._1);
        if (v.tag === "LT") { return Data$dOrdering.LT; }
        if (v.tag === "GT") { return Data$dOrdering.GT; }
        const v1 = Data$dOrd.ordBoolean.compare(x._2)(y._2);
        if (v1.tag === "LT") { return Data$dOrdering.LT; }
        if (v1.tag === "GT") { return Data$dOrdering.GT; }
        const v2 = Data$dOrd.ordInt.compare(x._3.a)(y._3.a);
        if (v2.tag === "LT") { return Data$dOrdering.LT; }
        if (v2.tag === "GT") { return Data$dOrdering.GT; }
        return Data$dOrd.ordBoolean.compare(x._3.b)(y._3.b);
      }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Ignored2") { return Data$dOrdering.GT; }
    if (x.tag === "Top2") {
      if (y.tag === "Top2") {
        const v = Data$dOrd.ordInt.compare(x._1)(y._1);
        if (v.tag === "LT") { return Data$dOrdering.LT; }
        if (v.tag === "GT") { return Data$dOrdering.GT; }
        const v1 = Data$dOrd.ordBoolean.compare(x._2)(y._2);
        if (v1.tag === "LT") { return Data$dOrdering.LT; }
        if (v1.tag === "GT") { return Data$dOrdering.GT; }
        const v2 = Data$dOrd.ordInt.compare(x._3.a)(y._3.a);
        if (v2.tag === "LT") { return Data$dOrdering.LT; }
        if (v2.tag === "GT") { return Data$dOrdering.GT; }
        return Data$dOrd.ordBoolean.compare(x._3.b)(y._3.b);
      }
      $runtime.fail();
    }
    $runtime.fail();
  },
  Eq0: () => eqTest2
};
const genericTest2 = {
  to: x => {
    if (x.tag === "Inl") { return $Test2("Bottom2", x._1._1, x._1._2._1, x._1._2._2); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $Test2("Ignored2", x._1._1._1, x._1._1._2._1, x._1._1._2._2); }
      if (x._1.tag === "Inr") { return $Test2("Top2", x._1._1._1, x._1._1._2._1, x._1._1._2._2); }
      $runtime.fail();
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Bottom2") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3))); }
    if (x.tag === "Ignored2") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3)))); }
    if (x.tag === "Top2") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3)))); }
    $runtime.fail();
  }
};
const boundedTest2 = {
  bottom: /* #__PURE__ */ $Test2("Bottom2", -2147483648, false, {a: -2147483648, b: false}),
  top: /* #__PURE__ */ $Test2("Top2", 2147483647, true, {a: 2147483647, b: true}),
  Ord0: () => ordTest2
};
export {$Test2, Bottom2, Ignored2, Top2, boundedTest2, eqTest2, genericTest2, ordTest2};

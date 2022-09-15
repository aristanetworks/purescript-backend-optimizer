// @inline export genericTest2.to arity=1
import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Test2 = (tag, _1) => ({tag, _1});
const Bottom2 = value0 => $Test2("Bottom2", value0);
const Ignored2 = /* #__PURE__ */ $Test2("Ignored2");
const Top2 = value0 => $Test2("Top2", value0);
const eqTest2 = {
  eq: x => y => {
    if (x.tag === "Bottom2") {
      if (y.tag === "Bottom2") { return x._1 === y._1; }
      return false;
    }
    if (x.tag === "Ignored2") { return y.tag === "Ignored2"; }
    if (x.tag === "Top2") {
      if (y.tag === "Top2") { return x._1 === y._1; }
      return false;
    }
    return false;
  }
};
const ordTest2 = {
  compare: x => y => {
    if (x.tag === "Bottom2") {
      if (y.tag === "Bottom2") { return Data$dOrd.ordInt.compare(x._1)(y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Bottom2") { return Data$dOrdering.GT; }
    if (x.tag === "Ignored2") {
      if (y.tag === "Ignored2") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Ignored2") { return Data$dOrdering.GT; }
    if (x.tag === "Top2") {
      if (y.tag === "Top2") { return Data$dOrd.ordInt.compare(x._1)(y._1); }
      $runtime.fail();
    }
    $runtime.fail();
  },
  Eq0: () => eqTest2
};
const genericTest2 = {
  to: x => {
    if (x.tag === "Inl") { return $Test2("Bottom2", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return Ignored2; }
      if (x._1.tag === "Inr") { return $Test2("Top2", x._1._1); }
      $runtime.fail();
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Bottom2") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "Ignored2") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "Top2") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", x._1)); }
    $runtime.fail();
  }
};
const boundedTest2 = {bottom: /* #__PURE__ */ $Test2("Bottom2", -2147483648), top: /* #__PURE__ */ $Test2("Top2", 2147483647), Ord0: () => ordTest2};
export {$Test2, Bottom2, Ignored2, Top2, boundedTest2, eqTest2, genericTest2, ordTest2};

// @inline export genericTest1.to arity=1
import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Test1 = tag => ({tag});
const Bottom1 = /* #__PURE__ */ $Test1("Bottom1");
const Ignored1 = /* #__PURE__ */ $Test1("Ignored1");
const Top1 = /* #__PURE__ */ $Test1("Top1");
const eqTest1 = {
  eq: x => y => {
    if (x.tag === "Bottom1") { return y.tag === "Bottom1"; }
    if (x.tag === "Ignored1") { return y.tag === "Ignored1"; }
    if (x.tag === "Top1") { return y.tag === "Top1"; }
    return false;
  }
};
const ordTest1 = {
  compare: x => y => {
    if (x.tag === "Bottom1") {
      if (y.tag === "Bottom1") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Bottom1") { return Data$dOrdering.GT; }
    if (x.tag === "Ignored1") {
      if (y.tag === "Ignored1") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Ignored1") { return Data$dOrdering.GT; }
    if (x.tag === "Top1") {
      if (y.tag === "Top1") { return Data$dOrdering.EQ; }
      $runtime.fail();
    }
    $runtime.fail();
  },
  Eq0: () => eqTest1
};
const genericTest1 = {
  to: x => {
    if (x.tag === "Inl") { return Bottom1; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return Ignored1; }
      if (x._1.tag === "Inr") { return Top1; }
      $runtime.fail();
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Bottom1") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x.tag === "Ignored1") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "Top1") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)); }
    $runtime.fail();
  }
};
const boundedTest1 = {bottom: Bottom1, top: Top1, Ord0: () => ordTest1};
export {$Test1, Bottom1, Ignored1, Top1, boundedTest1, eqTest1, genericTest1, ordTest1};

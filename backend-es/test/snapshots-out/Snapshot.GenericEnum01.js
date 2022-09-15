// @inline export genericTest1.to arity=1
// @inline export genericTest1.from arity=1
import * as $runtime from "../runtime.js";
import * as Data$dEnum$dGeneric from "../Data.Enum.Generic/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Test1 = tag => ({tag});
const genericBottomConstructor = {"genericBottom'": Data$dGeneric$dRep.NoArguments};
const genericBottomSum = {"genericBottom'": /* #__PURE__ */ Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)};
const genericTopConstructor = {"genericTop'": Data$dGeneric$dRep.NoArguments};
const genericEnumConstructor = /* #__PURE__ */ Data$dEnum$dGeneric.genericEnumConstructor(Data$dEnum$dGeneric.genericEnumNoArguments);
const genericEnumSum1 = /* #__PURE__ */ Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(/* #__PURE__ */ Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(/* #__PURE__ */ Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(genericEnumConstructor)(genericBottomConstructor))(genericBottomSum))(genericBottomSum);
const A1 = /* #__PURE__ */ $Test1("A1");
const B1 = /* #__PURE__ */ $Test1("B1");
const C1 = /* #__PURE__ */ $Test1("C1");
const D1 = /* #__PURE__ */ $Test1("D1");
const eqTest1 = {
  eq: x => y => {
    if (x.tag === "A1") { return y.tag === "A1"; }
    if (x.tag === "B1") { return y.tag === "B1"; }
    if (x.tag === "C1") { return y.tag === "C1"; }
    if (x.tag === "D1") { return y.tag === "D1"; }
    return false;
  }
};
const ordTest1 = {
  compare: x => y => {
    if (x.tag === "A1") {
      if (y.tag === "A1") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "A1") { return Data$dOrdering.GT; }
    if (x.tag === "B1") {
      if (y.tag === "B1") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "B1") { return Data$dOrdering.GT; }
    if (x.tag === "C1") {
      if (y.tag === "C1") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y.tag === "C1") { return Data$dOrdering.GT; }
    if (x.tag === "D1") {
      if (y.tag === "D1") { return Data$dOrdering.EQ; }
      $runtime.fail();
    }
    $runtime.fail();
  },
  Eq0: () => eqTest1
};
const genericTest1 = {
  to: x => {
    if (x.tag === "Inl") { return A1; }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return B1; }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return C1; }
        if (x._1._1.tag === "Inr") { return D1; }
        $runtime.fail();
      }
      $runtime.fail();
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "A1") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x.tag === "B1") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "C1") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))); }
    if (x.tag === "D1") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments))); }
    $runtime.fail();
  }
};
const boundedTest1 = {bottom: A1, top: D1, Ord0: () => ordTest1};
const enumTest1 = {
  pred: /* #__PURE__ */ Data$dEnum$dGeneric.genericPred(genericTest1)(genericEnumSum1),
  succ: /* #__PURE__ */ Data$dEnum$dGeneric.genericSucc(genericTest1)(genericEnumSum1),
  Ord0: () => ordTest1
};
export {
  $Test1,
  A1,
  B1,
  C1,
  D1,
  boundedTest1,
  enumTest1,
  eqTest1,
  genericBottomConstructor,
  genericBottomSum,
  genericEnumConstructor,
  genericEnumSum1,
  genericTest1,
  genericTopConstructor,
  ordTest1
};

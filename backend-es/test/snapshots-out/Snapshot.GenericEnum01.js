// @inline export genericTest1.to arity=1
// @inline export genericTest1.from arity=1
import * as $runtime from "../runtime.js";
import * as Data$dEnum$dGeneric from "../Data.Enum.Generic/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Test1 = tag => ({tag});
const genericBottomConstructor = {"genericBottom'": Data$dGeneric$dRep.NoArguments};
const genericBottomSum = {"genericBottom'": /* #__PURE__ */ Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)};
const genericTopConstructor = {"genericTop'": Data$dGeneric$dRep.NoArguments};
const genericEnumConstructor = /* #__PURE__ */ Data$dEnum$dGeneric.genericEnumConstructor(Data$dEnum$dGeneric.genericEnumNoArguments);
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
  pred: x => {
    const $1 = Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(genericEnumConstructor)(genericBottomConstructor))(genericBottomSum);
    if (x.tag === "A1") { return Data$dMaybe.Nothing; }
    if (x.tag === "B1") {
      const v1 = $1["genericPred'"](Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments));
      if (v1.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", A1); }
      if (v1.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            if (v1._1.tag === "Inl") { return B1; }
            if (v1._1.tag === "Inr") {
              if (v1._1._1.tag === "Inl") { return C1; }
              if (v1._1._1.tag === "Inr") { return D1; }
              $runtime.fail();
            }
            $runtime.fail();
          })()
        );
      }
      $runtime.fail();
    }
    if (x.tag === "C1") {
      const v1 = $1["genericPred'"](Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)));
      if (v1.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", A1); }
      if (v1.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            if (v1._1.tag === "Inl") { return B1; }
            if (v1._1.tag === "Inr") {
              if (v1._1._1.tag === "Inl") { return C1; }
              if (v1._1._1.tag === "Inr") { return D1; }
              $runtime.fail();
            }
            $runtime.fail();
          })()
        );
      }
      $runtime.fail();
    }
    if (x.tag === "D1") {
      const v1 = $1["genericPred'"](Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)));
      if (v1.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", A1); }
      if (v1.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            if (v1._1.tag === "Inl") { return B1; }
            if (v1._1.tag === "Inr") {
              if (v1._1._1.tag === "Inl") { return C1; }
              if (v1._1._1.tag === "Inr") { return D1; }
              $runtime.fail();
            }
            $runtime.fail();
          })()
        );
      }
      $runtime.fail();
    }
    $runtime.fail();
  },
  succ: x => {
    const $1 = Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(Data$dEnum$dGeneric.genericEnumSum(genericEnumConstructor)(genericTopConstructor)(genericEnumConstructor)(genericBottomConstructor))(genericBottomSum);
    if (x.tag === "A1") { return Data$dMaybe.$Maybe("Just", B1); }
    if (x.tag === "B1") {
      const $2 = $1["genericSucc'"](Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments));
      if ($2.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            if ($2._1.tag === "Inl") { return B1; }
            if ($2._1.tag === "Inr") {
              if ($2._1._1.tag === "Inl") { return C1; }
              if ($2._1._1.tag === "Inr") { return D1; }
              $runtime.fail();
            }
            $runtime.fail();
          })()
        );
      }
      return Data$dMaybe.Nothing;
    }
    if (x.tag === "C1") {
      const $2 = $1["genericSucc'"](Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)));
      if ($2.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            if ($2._1.tag === "Inl") { return B1; }
            if ($2._1.tag === "Inr") {
              if ($2._1._1.tag === "Inl") { return C1; }
              if ($2._1._1.tag === "Inr") { return D1; }
              $runtime.fail();
            }
            $runtime.fail();
          })()
        );
      }
      return Data$dMaybe.Nothing;
    }
    if (x.tag === "D1") {
      const $2 = $1["genericSucc'"](Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments)));
      if ($2.tag === "Just") {
        return Data$dMaybe.$Maybe(
          "Just",
          (() => {
            if ($2._1.tag === "Inl") { return B1; }
            if ($2._1.tag === "Inr") {
              if ($2._1._1.tag === "Inl") { return C1; }
              if ($2._1._1.tag === "Inr") { return D1; }
              $runtime.fail();
            }
            $runtime.fail();
          })()
        );
      }
      return Data$dMaybe.Nothing;
    }
    $runtime.fail();
  },
  Ord0: () => ordTest1
};
export {$Test1, A1, B1, C1, D1, boundedTest1, enumTest1, eqTest1, genericBottomConstructor, genericBottomSum, genericEnumConstructor, genericTest1, genericTopConstructor, ordTest1};

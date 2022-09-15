// @inline export genericTest4.to arity=1
import * as $runtime from "../runtime.js";
import * as Data$dBounded from "../Data.Bounded/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Snapshot$dGenericBounded01 from "../Snapshot.GenericBounded01/index.js";
import * as Snapshot$dGenericBounded02 from "../Snapshot.GenericBounded02/index.js";
import * as Snapshot$dGenericBounded03 from "../Snapshot.GenericBounded03/index.js";
const $Test4 = (tag, _1, _2, _3, _4, _5, _6, _7) => ({tag, _1, _2, _3, _4, _5, _6, _7});
const Bottom4 = value0 => value1 => value2 => value3 => value4 => value5 => value6 => $Test4("Bottom4", value0, value1, value2, value3, value4, value5, value6);
const Ignored4 = /* #__PURE__ */ $Test4("Ignored4");
const Top4 = value0 => value1 => value2 => value3 => value4 => value5 => value6 => $Test4("Top4", value0, value1, value2, value3, value4, value5, value6);
const eqTest4 = dictEq1 => dictEq => {
  const eq12 = dictEq1.eq1(dictEq);
  return {
    eq: x => y => {
      if (x.tag === "Bottom4") {
        if (y.tag === "Bottom4") {
          return x._1 === y._1 && x._2.a === y._2.a && (() => {
            if (x._3.tag === "Bottom1") { return y._3.tag === "Bottom1"; }
            if (x._3.tag === "Ignored1") { return y._3.tag === "Ignored1"; }
            if (x._3.tag === "Top1") { return y._3.tag === "Top1"; }
            return false;
          })() && Snapshot$dGenericBounded02.eqTest2.eq(x._4)(y._4) && Snapshot$dGenericBounded03.eqTest3.eq(x._5)(y._5) && dictEq.eq(x._6)(y._6) && eq12(x._7)(y._7);
        }
        return false;
      }
      if (x.tag === "Ignored4") { return y.tag === "Ignored4"; }
      if (x.tag === "Top4") {
        if (y.tag === "Top4") {
          return x._1 === y._1 && x._2.a === y._2.a && (() => {
            if (x._3.tag === "Bottom1") { return y._3.tag === "Bottom1"; }
            if (x._3.tag === "Ignored1") { return y._3.tag === "Ignored1"; }
            if (x._3.tag === "Top1") { return y._3.tag === "Top1"; }
            return false;
          })() && Snapshot$dGenericBounded02.eqTest2.eq(x._4)(y._4) && Snapshot$dGenericBounded03.eqTest3.eq(x._5)(y._5) && dictEq.eq(x._6)(y._6) && eq12(x._7)(y._7);
        }
        return false;
      }
      return false;
    }
  };
};
const ordTest4 = dictOrd1 => {
  const eqTest41 = eqTest4(dictOrd1.Eq10());
  return dictOrd => {
    const compare12 = dictOrd1.compare1(dictOrd);
    const eqTest42 = eqTest41(dictOrd.Eq0());
    return {
      compare: x => y => {
        if (x.tag === "Bottom4") {
          if (y.tag === "Bottom4") {
            const v = Data$dOrd.ordInt.compare(x._1)(y._1);
            if (v.tag === "LT") { return Data$dOrdering.LT; }
            if (v.tag === "GT") { return Data$dOrdering.GT; }
            const v1 = Data$dOrd.ordInt.compare(x._2.a)(y._2.a);
            if (v1.tag === "LT") { return Data$dOrdering.LT; }
            if (v1.tag === "GT") { return Data$dOrdering.GT; }
            const v2 = Snapshot$dGenericBounded01.ordTest1.compare(x._3)(y._3);
            if (v2.tag === "LT") { return Data$dOrdering.LT; }
            if (v2.tag === "GT") { return Data$dOrdering.GT; }
            const v3 = Snapshot$dGenericBounded02.ordTest2.compare(x._4)(y._4);
            if (v3.tag === "LT") { return Data$dOrdering.LT; }
            if (v3.tag === "GT") { return Data$dOrdering.GT; }
            const v4 = Snapshot$dGenericBounded03.ordTest3.compare(x._5)(y._5);
            if (v4.tag === "LT") { return Data$dOrdering.LT; }
            if (v4.tag === "GT") { return Data$dOrdering.GT; }
            const v5 = dictOrd.compare(x._6)(y._6);
            if (v5.tag === "LT") { return Data$dOrdering.LT; }
            if (v5.tag === "GT") { return Data$dOrdering.GT; }
            return compare12(x._7)(y._7);
          }
          return Data$dOrdering.LT;
        }
        if (y.tag === "Bottom4") { return Data$dOrdering.GT; }
        if (x.tag === "Ignored4") {
          if (y.tag === "Ignored4") { return Data$dOrdering.EQ; }
          return Data$dOrdering.LT;
        }
        if (y.tag === "Ignored4") { return Data$dOrdering.GT; }
        if (x.tag === "Top4") {
          if (y.tag === "Top4") {
            const v = Data$dOrd.ordInt.compare(x._1)(y._1);
            if (v.tag === "LT") { return Data$dOrdering.LT; }
            if (v.tag === "GT") { return Data$dOrdering.GT; }
            const v1 = Data$dOrd.ordInt.compare(x._2.a)(y._2.a);
            if (v1.tag === "LT") { return Data$dOrdering.LT; }
            if (v1.tag === "GT") { return Data$dOrdering.GT; }
            const v2 = Snapshot$dGenericBounded01.ordTest1.compare(x._3)(y._3);
            if (v2.tag === "LT") { return Data$dOrdering.LT; }
            if (v2.tag === "GT") { return Data$dOrdering.GT; }
            const v3 = Snapshot$dGenericBounded02.ordTest2.compare(x._4)(y._4);
            if (v3.tag === "LT") { return Data$dOrdering.LT; }
            if (v3.tag === "GT") { return Data$dOrdering.GT; }
            const v4 = Snapshot$dGenericBounded03.ordTest3.compare(x._5)(y._5);
            if (v4.tag === "LT") { return Data$dOrdering.LT; }
            if (v4.tag === "GT") { return Data$dOrdering.GT; }
            const v5 = dictOrd.compare(x._6)(y._6);
            if (v5.tag === "LT") { return Data$dOrdering.LT; }
            if (v5.tag === "GT") { return Data$dOrdering.GT; }
            return compare12(x._7)(y._7);
          }
          $runtime.fail();
        }
        $runtime.fail();
      },
      Eq0: () => eqTest42
    };
  };
};
const genericTest4 = {
  to: x => {
    if (x.tag === "Inl") { return $Test4("Bottom4", x._1._1, x._1._2._1, x._1._2._2._1, x._1._2._2._2._1, x._1._2._2._2._2._1, x._1._2._2._2._2._2._1, x._1._2._2._2._2._2._2); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return Ignored4; }
      if (x._1.tag === "Inr") {
        return $Test4("Top4", x._1._1._1, x._1._1._2._1, x._1._1._2._2._1, x._1._1._2._2._2._1, x._1._1._2._2._2._2._1, x._1._1._2._2._2._2._2._1, x._1._1._2._2._2._2._2._2);
      }
      $runtime.fail();
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Bottom4") {
      return Data$dGeneric$dRep.$Sum(
        "Inl",
        Data$dGeneric$dRep.$Product(
          x._1,
          Data$dGeneric$dRep.$Product(
            x._2,
            Data$dGeneric$dRep.$Product(x._3, Data$dGeneric$dRep.$Product(x._4, Data$dGeneric$dRep.$Product(x._5, Data$dGeneric$dRep.$Product(x._6, x._7))))
          )
        )
      );
    }
    if (x.tag === "Ignored4") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "Top4") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Product(
            x._1,
            Data$dGeneric$dRep.$Product(
              x._2,
              Data$dGeneric$dRep.$Product(x._3, Data$dGeneric$dRep.$Product(x._4, Data$dGeneric$dRep.$Product(x._5, Data$dGeneric$dRep.$Product(x._6, x._7))))
            )
          )
        )
      );
    }
    $runtime.fail();
  }
};
const boundedTest4 = dictOrd1 => {
  const ordTest41 = ordTest4(dictOrd1);
  return dictBounded => dictBounded1 => {
    const ordTest42 = ordTest41(dictBounded1.Ord0());
    return {
      bottom: $Test4(
        "Bottom4",
        -2147483648,
        {a: -2147483648},
        Snapshot$dGenericBounded01.Bottom1,
        Snapshot$dGenericBounded02.boundedTest2.bottom,
        Snapshot$dGenericBounded03.boundedTest3.bottom,
        dictBounded1.bottom,
        dictBounded.bottom
      ),
      top: $Test4(
        "Top4",
        2147483647,
        {a: 2147483647},
        Snapshot$dGenericBounded01.Top1,
        Snapshot$dGenericBounded02.boundedTest2.top,
        Snapshot$dGenericBounded03.boundedTest3.top,
        dictBounded1.top,
        dictBounded.top
      ),
      Ord0: () => ordTest42
    };
  };
};
const boundedTest41 = /* #__PURE__ */ boundedTest4(/* #__PURE__ */ Data$dTuple.ord1Tuple(Data$dOrd.ordInt))(/* #__PURE__ */ Data$dTuple.boundedTuple(Data$dBounded.boundedInt)(Data$dBounded.boundedInt))(Data$dBounded.boundedInt);
const test5 = /* #__PURE__ */ (() => boundedTest41.bottom)();
const test6 = /* #__PURE__ */ (() => boundedTest41.top)();
export {$Test4, Bottom4, Ignored4, Top4, boundedTest4, boundedTest41, eqTest4, genericTest4, ordTest4, test5, test6};

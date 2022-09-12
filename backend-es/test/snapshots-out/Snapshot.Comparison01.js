import * as $runtime from "../runtime.js";
import * as Data$dComparison from "../Data.Comparison/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const defaultComparison1 = /* #__PURE__ */ (() => Data$dOrd.ordRecord()((() => {
  const eqRowCons2 = {eqRecord: v => ra => rb => ra.foo === rb.foo};
  return {
    compareRecord: v => ra => rb => {
      const left = Data$dOrd.ordInt.compare(ra.foo)(rb.foo);
      if (left.tag === "LT" || (left.tag === "GT" || !(left.tag === "EQ"))) { return left; }
      return Data$dOrdering.EQ;
    },
    EqRecord0: () => eqRowCons2
  };
})()).compare)();
const test8 = /* #__PURE__ */ (() => Data$dComparison.monoidComparison.mempty)();
const test7 = /* #__PURE__ */ (() => {
  const defCom = Data$dTuple.ordTuple(Data$dOrd.ordInt)(Data$dTuple.ordTuple(Data$dOrd.ordString)(Data$dOrd.ordBoolean)).compare;
  return Data$dComparison.semigroupComparison.append(x => y => defCom(x._1)(y._1))(Data$dComparison.semigroupComparison.append(x => y => defCom(x._2._1)(y._2._1))(x => y => defCom(x._2._2)(y._2._2)));
})();
const test6 = x => y => Data$dOrd.ordInt.compare(x.foo)(y.foo);
const test5 = /* #__PURE__ */ (() => Data$dOrd.ordInt.compare)();
const test4 = /* #__PURE__ */ (() => Data$dComparison.monoidComparison.mempty)();
const test3 = /* #__PURE__ */ (() => {
  const defCom = Data$dOrd.ordRecord()((() => {
    const eqRowCons2 = {eqRecord: v => ra => rb => ra.bar === rb.bar && (ra.baz === rb.baz && ra.foo === rb.foo)};
    return {
      compareRecord: v => ra => rb => {
        const left = Data$dOrd.ordString.compare(ra.bar)(rb.bar);
        if (left.tag === "LT" || (left.tag === "GT" || !(left.tag === "EQ"))) { return left; }
        const left$1 = Data$dOrd.ordBoolean.compare(ra.baz)(rb.baz);
        if (left$1.tag === "LT" || (left$1.tag === "GT" || !(left$1.tag === "EQ"))) { return left$1; }
        const left$2 = Data$dOrd.ordInt.compare(ra.foo)(rb.foo);
        if (left$2.tag === "LT" || (left$2.tag === "GT" || !(left$2.tag === "EQ"))) { return left$2; }
        return Data$dOrdering.EQ;
      },
      EqRecord0: () => eqRowCons2
    };
  })()).compare;
  return Data$dComparison.semigroupComparison.append(x => y => defCom(x.foo)(y.foo))(Data$dComparison.semigroupComparison.append(x => y => defCom(x.bar)(y.bar))(x => y => defCom(x.baz)(y.baz)));
})();
const test2 = x => y => defaultComparison1(x.foo)(y.foo);
const test1 = defaultComparison1;
export {defaultComparison1, test1, test2, test3, test4, test5, test6, test7, test8};

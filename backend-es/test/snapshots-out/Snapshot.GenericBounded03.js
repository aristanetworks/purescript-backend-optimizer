import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Test3 = (_1, _2) => ({tag: "Both3", _1, _2});
const Both3 = value0 => value1 => $Test3(value0, value1);
const eqTest3 = {eq: x => y => x._1 === y._1 && x._2 === y._2};
const ordTest3 = {
  compare: x => y => {
    const v = Data$dOrd.ordInt.compare(x._1)(y._1);
    if (v.tag === "LT") { return Data$dOrdering.LT; }
    if (v.tag === "GT") { return Data$dOrdering.GT; }
    return Data$dOrd.ordInt.compare(x._2)(y._2);
  },
  Eq0: () => eqTest3
};
const genericTest3 = {to: x => $Test3(x._1, x._2), from: x => Data$dGeneric$dRep.$Product(x._1, x._2)};
const boundedTest3 = {bottom: /* #__PURE__ */ $Test3(-2147483648, -2147483648), top: /* #__PURE__ */ $Test3(2147483647, 2147483647), Ord0: () => ordTest3};
export {$Test3, Both3, boundedTest3, eqTest3, genericTest3, ordTest3};

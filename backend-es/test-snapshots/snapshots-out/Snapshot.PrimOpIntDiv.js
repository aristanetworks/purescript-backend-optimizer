// @inline export divNoInline never
import * as $runtime from "../runtime.js";
import * as Assert from "../Assert/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const divNoInline = a => b => $runtime.intDiv(a, b);
const main = /* #__PURE__ */ (() => {
  const $0 = Assert.assertEqual(Data$dEq.eqInt)(Data$dShow.showInt)("div1")({expected: 0, actual: divNoInline(1)(0)});
  return () => {
    $0();
    Assert.assertEqual(Data$dEq.eqInt)(Data$dShow.showInt)("div2")({expected: 1, actual: divNoInline(3)(2)})();
    return Assert.assertEqual(Data$dEq.eqInt)(Data$dShow.showInt)("div3")({expected: -1, actual: divNoInline(3)(-2)})();
  };
})();
export {divNoInline, main};

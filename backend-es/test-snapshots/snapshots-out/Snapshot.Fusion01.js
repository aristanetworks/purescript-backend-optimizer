// @inline export overArray arity=1
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const toUnfoldable = /* #__PURE__ */ (() => Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
  if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
  $runtime.fail();
}))();
const test = x => Data$dArray.reverse(toUnfoldable((() => {
  const loop = loop$a0$copy => loop$a1$copy => {
    let loop$a0 = loop$a0$copy, loop$a1 = loop$a1$copy, loop$c = true, loop$r;
    while (loop$c) {
      const n = loop$a0, acc = loop$a1;
      if (n === 0) {
        loop$c = false;
        loop$r = acc;
        continue;
      }
      loop$a0 = n - 1 | 0;
      loop$a1 = (() => {
        const v1 = Data$dString$dCodeUnits.stripPrefix("1")(Data$dShow.showIntImpl(1 + x[n] | 0));
        if (v1.tag === "Just") {
          const $0 = "2" + v1._1;
          if ($0 !== "wat") { return Data$dList$dTypes.$List("Cons", $0 + "1", acc); }
          return acc;
        }
        if (v1.tag === "Nothing") { return acc; }
        $runtime.fail();
      })();
    }
    return loop$r;
  };
  return loop(x.length - 1 | 0)(Data$dList$dTypes.Nil);
})()));
export {test, toUnfoldable};

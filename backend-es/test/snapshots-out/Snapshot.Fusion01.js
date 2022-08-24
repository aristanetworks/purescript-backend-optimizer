// @inline export overArray arity=1
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const toUnfoldable = /* #__PURE__ */ Data$dList.toUnfoldable(Data$dUnfoldable.unfoldableArray);
const test = x => Data$dArray.reverse(toUnfoldable((() => {
  const loop = loop$0$copy => loop$1$copy => {
    let loop$0 = loop$0$copy, loop$1 = loop$1$copy, loop$c = true, loop$r;
    while (loop$c) {
      const n = loop$0, acc = loop$1;
      if (n === 0) {
        loop$c = false;
        loop$r = acc;
        continue;
      }
      loop$0 = n - 1 | 0;
      loop$1 = (() => {
        const v1 = Data$dString$dCodeUnits.stripPrefix("1")(Data$dShow.showIntImpl(1 + x[n] | 0));
        if (v1.tag === "Just") {
          const $5 = "2" + v1._1;
          if ($5 !== "wat") { return Data$dList$dTypes.$List("Cons", $5 + "1", acc); }
          return acc;
        }
        if (v1.tag === "Nothing") { return acc; }
        $runtime.fail();
      })();
      continue;
    };
    return loop$r;
  };
  return loop(x.length - 1 | 0)(Data$dList$dTypes.Nil);
})()));
export {test, toUnfoldable};

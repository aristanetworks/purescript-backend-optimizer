// @inline export mapU arity=1
// @inline export filterMapU arity=1
// @inline export filterU arity=1
// @inline export fromArray arity=1
// @inline export toArray arity=1
// @inline export overArray arity=1
import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const toUnfoldable = /* #__PURE__ */ Data$dList.toUnfoldable(Data$dUnfoldable.unfoldableArray);
const test = x => {
  const loop = loop$0$copy => loop$1$copy => {
    let loop$0 = loop$0$copy, loop$1 = loop$1$copy, loop$c = true, loop$r;
    while (loop$c) {
      const acc = loop$0, s$p = loop$1;
      const loop_1 = loop_1$0$copy => {
        let loop_1$0 = loop_1$0$copy, loop_1$c = true, loop_1$r;
        while (loop_1$c) {
          const s$p$p = loop_1$0;
          const loop_2 = loop_2$0$copy => {
            let loop_2$0 = loop_2$0$copy, loop_2$c = true, loop_2$r;
            while (loop_2$c) {
              const s$p$p_1 = loop_2$0;
              if (s$p$p_1 === x.length) {
                loop$c = loop_1$c = loop_2$c = false;
                loop$r = Data$dArray.reverse(toUnfoldable(acc));
                continue;
              }
              const _8 = s$p$p_1 + 1 | 0;
              const v1 = Data$dString$dCodeUnits.stripPrefix("1")(Data$dShow.showIntImpl(1 + x[s$p$p_1] | 0));
              if (v1.tag === "Nothing") {
                loop_2$0 = _8;
                continue;
              }
              if (v1.tag === "Just") {
                const _10 = "2" + v1._1;
                if (_10 !== "wat") {
                  loop_1$c = loop_2$c = false;
                  loop$0 = Data$dList$dTypes.$List("Cons", _10 + "1", acc);
                  loop$1 = _8;
                  continue;
                }
                loop_2$c = false;
                loop_1$0 = _8;
                continue;
              }
              $runtime.fail();
            };
            return loop_2$r;
          };
          loop_2(s$p$p);
          continue;
        };
        return loop_1$r;
      };
      loop_1(s$p);
      continue;
    };
    return loop$r;
  };
  return loop(Data$dList$dTypes.Nil)(0);
};
export {test, toUnfoldable};

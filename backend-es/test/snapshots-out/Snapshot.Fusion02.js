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
  const loop = loop$a0$copy => loop$a1$copy => {
    let loop$a0 = loop$a0$copy, loop$a1 = loop$a1$copy, loop$c = true, loop$r;
    while (loop$c) {
      const s2 = loop$a0, acc = loop$a1;
      const loop$1 = loop$1$a0$copy => {
        let loop$1$a0 = loop$1$a0$copy, loop$1$c = true, loop$1$r;
        while (loop$1$c) {
          const s3 = loop$1$a0;
          const loop$2 = loop$2$a0$copy => {
            let loop$2$a0 = loop$2$a0$copy, loop$2$c = true, loop$2$r;
            while (loop$2$c) {
              const s3$1 = loop$2$a0;
              if (s3$1 === x.length) {
                loop$c = loop$1$c = loop$2$c = false;
                loop$r = Data$dArray.reverse(toUnfoldable(acc));
                continue;
              }
              const $0 = s3$1 + 1 | 0;
              const v1 = Data$dString$dCodeUnits.stripPrefix("1")(Data$dShow.showIntImpl(1 + x[s3$1] | 0));
              if (v1.tag === "Nothing") {
                loop$2$a0 = $0;
                continue;
              }
              if (v1.tag === "Just") {
                const $1 = "2" + v1._1;
                if ($1 !== "wat") {
                  loop$1$c = loop$2$c = false;
                  loop$a0 = $0;
                  loop$a1 = Data$dList$dTypes.$List("Cons", $1 + "1", acc);
                  continue;
                }
                loop$2$c = false;
                loop$1$a0 = $0;
                continue;
              }
              $runtime.fail();
            }
            return loop$2$r;
          };
          loop$2(s3);
        }
        return loop$1$r;
      };
      loop$1(s2);
    }
    return loop$r;
  };
  return loop(0)(Data$dList$dTypes.Nil);
};
export {test, toUnfoldable};

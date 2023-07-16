import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test1 = /* #__PURE__ */ Data$dTuple.$Tuple(
  1,
  /* #__PURE__ */ (() => {
    const b = Effect$dConsole.log("foo");
    const a = Data$dFoldable.foldlArray(a => b$1 => Data$dEither.applyEither.apply((() => {
      if (a.tag === "Left") { return Data$dEither.$Either("Left", a._1); }
      if (a.tag === "Right") {
        const $0 = a._1;
        return Data$dEither.$Either("Right", $1 => $0 + $1 | 0);
      }
      $runtime.fail();
    })())(b$1))(Data$dEither.$Either("Right", 3))([Data$dEither.$Either("Left", b), Data$dEither.$Either("Right", 4), Data$dEither.$Either("Left", b)]);
    return Data$dTuple.$Tuple(
      a,
      (() => {
        if (a.tag === "Left") { return Data$dEither.$Either("Left", a._1); }
        if (a.tag === "Right") { return Data$dEither.$Either("Right", a._1 + 1 | 0); }
        $runtime.fail();
      })()
    );
  })()
);
export {test1};

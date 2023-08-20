import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test1$1$a = /* #__PURE__ */ Data$dFoldable.foldlArray(a => b => {
  if (a.tag === "Left") { return Data$dEither.$Either("Left", a._1); }
  if (a.tag === "Right") {
    if (b.tag === "Left") { return Data$dEither.$Either("Left", b._1); }
    if (b.tag === "Right") { return Data$dEither.$Either("Right", a._1 + b._1 | 0); }
  }
  $runtime.fail();
})(/* #__PURE__ */ Data$dEither.$Either("Right", 3))([
  /* #__PURE__ */ Data$dEither.$Either("Left", test1$0$b),
  /* #__PURE__ */ Data$dEither.$Either("Right", 4),
  /* #__PURE__ */ Data$dEither.$Either("Left", test1$0$b)
]);
const test1$0$b = /* #__PURE__ */ Effect$dConsole.log("foo");
const test1 = /* #__PURE__ */ Data$dTuple.$Tuple(
  1,
  /* #__PURE__ */ Data$dTuple.$Tuple(
    test1$1$a,
    /* #__PURE__ */ (() => {
      if (test1$1$a.tag === "Left") { return Data$dEither.$Either("Left", test1$1$a._1); }
      if (test1$1$a.tag === "Right") { return Data$dEither.$Either("Right", test1$1$a._1 + 1 | 0); }
      $runtime.fail();
    })()
  )
);
export {test1, test1$0$b, test1$1$a};

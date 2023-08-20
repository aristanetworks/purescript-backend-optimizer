import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const snd = a => a[1];
const right = x => {
  if (x.tag === "Right") { return x._1; }
  $runtime.fail();
};
const left = x => {
  if (x.tag === "Left") { return x._1; }
  $runtime.fail();
};
const fst = a => a[0];
const test1$0$b = /* #__PURE__ */ Effect$dConsole.log("foo");
const test1 = [
  /* #__PURE__ */ Data$dEither.$Either("Left", 5),
  /* #__PURE__ */ Data$dEither.$Either(
    "Right",
    [
      /* #__PURE__ */ Data$dEither.$Either(
        "Left",
        [
          /* #__PURE__ */ Data$dEither.$Either("Left", 1),
          /* #__PURE__ */ Data$dEither.$Either(
            "Right",
            [
              /* #__PURE__ */ Data$dEither.$Either("Left", test1$0$b),
              /* #__PURE__ */ Data$dEither.$Either("Right", [/* #__PURE__ */ Data$dEither.$Either("Left", 5), /* #__PURE__ */ Data$dEither.$Either("Right", test1$0$b)])
            ]
          )
        ]
      ),
      /* #__PURE__ */ Data$dEither.$Either("Right", 6)
    ]
  )
];
export {fst, left, right, snd, test1, test1$0$b};
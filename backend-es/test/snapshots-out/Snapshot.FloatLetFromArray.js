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
const test1 = /* #__PURE__ */ (() => {
  const b = Effect$dConsole.log("foo");
  return [
    Data$dEither.$Either("Left", 5),
    Data$dEither.$Either(
      "Right",
      [
        Data$dEither.$Either(
          "Left",
          [
            Data$dEither.$Either("Left", 1),
            Data$dEither.$Either("Right", [Data$dEither.$Either("Left", b), Data$dEither.$Either("Right", [Data$dEither.$Either("Left", 5), Data$dEither.$Either("Right", b)])])
          ]
        ),
        Data$dEither.$Either("Right", 6)
      ]
    )
  ];
})();
export {fst, left, right, snd, test1};

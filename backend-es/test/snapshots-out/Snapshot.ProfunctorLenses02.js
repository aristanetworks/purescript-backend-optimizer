// @inline Data.Lens.Internal.Forget.choiceForget arity=1
import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const test4 = a => {
  if (a.tag === "Left") {
    if (a._1.tag === "Left") { return Data$dMaybe.Nothing; }
    if (a._1.tag === "Right") { return Data$dMaybe.$Maybe("Just", a._1._1); }
    $runtime.fail();
  }
  if (a.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const test3 = x => {
  if (x.tag === "Left") {
    if (x._1.tag === "Left") { return Data$dMaybe.Nothing; }
    if (x._1.tag === "Right") { return Data$dMaybe.$Maybe("Just", x._1._1); }
    $runtime.fail();
  }
  if (x.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const test2 = a => {
  if (a.tag === "Left") { return Data$dMaybe.$Maybe("Just", a._1); }
  if (a.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const test1 = x => {
  if (x.tag === "Left") { return Data$dMaybe.$Maybe("Just", x._1); }
  if (x.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
export {test1, test2, test3, test4};

// @inline Data.Lens.Internal.Forget.choiceForget(..).left arity=1
// @inline Data.Lens.Internal.Forget.choiceForget(..).right arity=1
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
const test3 = v2 => {
  if (v2.tag === "Left") {
    if (v2._1.tag === "Left") { return Data$dMaybe.Nothing; }
    if (v2._1.tag === "Right") { return Data$dMaybe.$Maybe("Just", v2._1._1); }
    $runtime.fail();
  }
  if (v2.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const test2 = a => {
  if (a.tag === "Left") { return Data$dMaybe.$Maybe("Just", a._1); }
  if (a.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
const test1 = v2 => {
  if (v2.tag === "Left") { return Data$dMaybe.$Maybe("Just", v2._1); }
  if (v2.tag === "Right") { return Data$dMaybe.Nothing; }
  $runtime.fail();
};
export {test1, test2, test3, test4};

// @inline export testArrayIndex never
import * as $runtime from "../runtime.js";
import * as Assert from "../Assert/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const assertEqual = /* #__PURE__ */ Assert.assertEqual({
  eq: x => y => {
    if (x.tag === "Nothing") { return y.tag === "Nothing"; }
    return x.tag === "Just" && y.tag === "Just" && x._1 === y._1;
  }
})({
  show: v => {
    if (v.tag === "Just") { return "(Just " + Data$dShow.showIntImpl(v._1) + ")"; }
    if (v.tag === "Nothing") { return "Nothing"; }
    $runtime.fail();
  }
});
const testArrayIndex = arr => ix => {
  if (ix >= 0 && ix < arr.length) { return Data$dMaybe.$Maybe("Just", arr[ix]); }
  return Data$dMaybe.Nothing;
};
const main$darray = [1, 2, 3];
const main$d0 = /* #__PURE__ */ assertEqual("index -1")({expected: Data$dMaybe.Nothing, actual: /* #__PURE__ */ testArrayIndex(main$darray)(-1)});
const main = () => {
  main$d0();
  assertEqual("index 0")({expected: Data$dMaybe.$Maybe("Just", 1), actual: testArrayIndex(main$darray)(0)})();
  assertEqual("index 1")({expected: Data$dMaybe.$Maybe("Just", 2), actual: testArrayIndex(main$darray)(1)})();
  assertEqual("index 2")({expected: Data$dMaybe.$Maybe("Just", 3), actual: testArrayIndex(main$darray)(2)})();
  return assertEqual("index 3")({expected: Data$dMaybe.Nothing, actual: testArrayIndex(main$darray)(3)})();
};
export {assertEqual, main, main$d0, main$darray, testArrayIndex};

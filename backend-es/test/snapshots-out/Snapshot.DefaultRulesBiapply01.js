import * as $runtime from "../runtime.js";
import * as Control$dBiapply from "../Control.Biapply/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const test4 = /* #__PURE__ */ Data$dTuple.$Tuple(6, "hello world");
const test3 = /* #__PURE__ */ Data$dTuple.$Tuple(3, "hello world");
const test2 = /* #__PURE__ */ Control$dBiapply.biapplySecond(Control$dBiapply.biapplyTuple)(/* #__PURE__ */ Data$dTuple.$Tuple("b", 2))(/* #__PURE__ */ Data$dTuple.$Tuple("a", 1));
const test1 = /* #__PURE__ */ Control$dBiapply.biapplyFirst(Control$dBiapply.biapplyTuple)(/* #__PURE__ */ Data$dTuple.$Tuple("b", 2))(/* #__PURE__ */ Data$dTuple.$Tuple("a", 1));
export {test1, test2, test3, test4};

import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import {mb} from "./foreign.js";
const test5 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", mb._1); }
  return Data$dMaybe.Nothing;
})();
const test4 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", 42); }
  return Data$dMaybe.Nothing;
})();
const test3 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", 42); }
  return Data$dMaybe.Nothing;
})();
const test2 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dUnit.unit); }
  return Data$dMaybe.Nothing;
})();
const test1 = /* #__PURE__ */ (() => {
  if (mb.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dShow.showIntImpl(mb._1)); }
  return Data$dMaybe.Nothing;
})();
export {test1, test2, test3, test4, test5};
export * from "./foreign.js";

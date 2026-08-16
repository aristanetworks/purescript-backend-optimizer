import * as $mData$dMaybe from "../Data.Maybe/index.js";
import * as $mData$dShow from "../Data.Show/index.js";
const test5 = mb => {
  if (mb.tag === "Just") { return $mData$dMaybe.$Maybe("Just", mb._1); }
  return $mData$dMaybe.Nothing;
};
const test4 = mb => {
  if (mb.tag === "Just") { return $mData$dMaybe.$Maybe("Just", 42); }
  return $mData$dMaybe.Nothing;
};
const test3 = mb => {
  if (mb.tag === "Just") { return $mData$dMaybe.$Maybe("Just", 42); }
  return $mData$dMaybe.Nothing;
};
const test2 = mb => {
  if (mb.tag === "Just") { return $mData$dMaybe.$Maybe("Just", undefined); }
  return $mData$dMaybe.Nothing;
};
const test1 = mb => {
  if (mb.tag === "Just") { return $mData$dMaybe.$Maybe("Just", $mData$dShow.showIntImpl(mb._1)); }
  return $mData$dMaybe.Nothing;
};
export {test1, test2, test3, test4, test5};

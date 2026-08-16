// @inline export fromString arity=1
import * as $mData$dMaybe from "../Data.Maybe/index.js";
const $Test = tag => tag;
const Foo = /* #__PURE__ */ $Test("Foo");
const Bar = /* #__PURE__ */ $Test("Bar");
const Baz = /* #__PURE__ */ $Test("Baz");
const Qux = /* #__PURE__ */ $Test("Qux");
const fromString = v => {
  if (v === "foo") { return $mData$dMaybe.$Maybe("Just", Foo); }
  if (v === "bar") { return $mData$dMaybe.$Maybe("Just", Bar); }
  if (v === "baz") { return $mData$dMaybe.$Maybe("Just", Baz); }
  if (v === "qux") { return $mData$dMaybe.$Maybe("Just", Qux); }
  return $mData$dMaybe.Nothing;
};
const test = a => {
  if (a === "foo") { return 1; }
  if (a === "bar") { return 2; }
  if (a === "baz") { return 3; }
  if (a === "qux") { return 4; }
  return 0;
};
export {$Test, Bar, Baz, Foo, Qux, fromString, test};

// @inline Data.Maybe.maybe arity=3
// @inline Data.Maybe.maybe' arity=3
import * as $runtime from "../runtime.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import {a, f, g} from "./foreign.js";
const test5 = z => {
  const _1 = g(1);
  if (z.tag === "Nothing") { return a + 1 | 0; }
  if (z.tag === "Just") { return _1(z._1); }
  $runtime.fail();
};
const test4 = z => {
  const _1 = g(1);
  if (z.tag === "Nothing") { return f(Data$dUnit.unit); }
  if (z.tag === "Just") { return _1(z._1); }
  $runtime.fail();
};
const test3 = z => {
  if (z.tag === "Nothing") { return f(Data$dUnit.unit); }
  if (z.tag === "Just") { return 1 + z._1 | 0; }
  $runtime.fail();
};
const test2 = z => {
  const _1 = f(Data$dUnit.unit);
  const _2 = g(1);
  if (z.tag === "Nothing") { return _1; }
  if (z.tag === "Just") { return _2(z._1); }
  $runtime.fail();
};
const test1 = z => {
  const _1 = f(Data$dUnit.unit);
  if (z.tag === "Nothing") { return _1; }
  if (z.tag === "Just") { return 1 + z._1 | 0; }
  $runtime.fail();
};
export {test1, test2, test3, test4, test5};
export * from "./foreign.js";

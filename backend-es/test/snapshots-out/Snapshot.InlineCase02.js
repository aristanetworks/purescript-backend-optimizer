// @inline Data.Maybe.maybe arity=3
// @inline Data.Maybe.maybe' arity=3
import * as $runtime from "../runtime.js";
import {a, f, g} from "./foreign.js";
const test5 = z => {
  const $1 = g(1);
  if (z.tag === "Nothing") { return a + 1 | 0; }
  if (z.tag === "Just") { return $1(z._1); }
  $runtime.fail();
};
const test4 = z => {
  const $1 = g(1);
  if (z.tag === "Nothing") { return f(); }
  if (z.tag === "Just") { return $1(z._1); }
  $runtime.fail();
};
const test3 = z => {
  if (z.tag === "Nothing") { return f(); }
  if (z.tag === "Just") { return 1 + z._1 | 0; }
  $runtime.fail();
};
const test2 = z => {
  const $1 = f();
  const $2 = g(1);
  if (z.tag === "Nothing") { return $1; }
  if (z.tag === "Just") { return $2(z._1); }
  $runtime.fail();
};
const test1 = z => {
  const $1 = f();
  if (z.tag === "Nothing") { return $1; }
  if (z.tag === "Just") { return 1 + z._1 | 0; }
  $runtime.fail();
};
export {test1, test2, test3, test4, test5};
export * from "./foreign.js";

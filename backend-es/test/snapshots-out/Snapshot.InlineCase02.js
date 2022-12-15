// @inline Data.Maybe.maybe arity=3
// @inline Data.Maybe.maybe' arity=3
import * as $runtime from "../runtime.js";
const test5 = a => g => z => {
  const $0 = g(1);
  if (z.tag === "Nothing") { return a + 1 | 0; }
  if (z.tag === "Just") { return $0(z._1); }
  $runtime.fail();
};
const test4 = f => g => z => {
  const $0 = g(1);
  if (z.tag === "Nothing") { return f(); }
  if (z.tag === "Just") { return $0(z._1); }
  $runtime.fail();
};
const test3 = f => z => {
  if (z.tag === "Nothing") { return f(); }
  if (z.tag === "Just") { return 1 + z._1 | 0; }
  $runtime.fail();
};
const test2 = f => g => z => {
  const $0 = f();
  const $1 = g(1);
  if (z.tag === "Nothing") { return $0; }
  if (z.tag === "Just") { return $1(z._1); }
  $runtime.fail();
};
const test1 = f => z => {
  const $0 = f();
  if (z.tag === "Nothing") { return $0; }
  if (z.tag === "Just") { return 1 + z._1 | 0; }
  $runtime.fail();
};
export {test1, test2, test3, test4, test5};

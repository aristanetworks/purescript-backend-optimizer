// @inline Data.Maybe.maybe arity=2
// @inline Data.Maybe.maybe' arity=2
import * as $runtime from "../runtime.js";
const test5 = a => g => {
  const $0 = g(1);
  return v2 => {
    if (v2.tag === "Nothing") { return a + 1 | 0; }
    if (v2.tag === "Just") { return $0(v2._1); }
    $runtime.fail();
  };
};
const test4 = f => g => {
  const $0 = g(1);
  return v2 => {
    if (v2.tag === "Nothing") { return f(); }
    if (v2.tag === "Just") { return $0(v2._1); }
    $runtime.fail();
  };
};
const test3 = f => v2 => {
  if (v2.tag === "Nothing") { return f(); }
  if (v2.tag === "Just") { return 1 + v2._1 | 0; }
  $runtime.fail();
};
const test2 = f => g => {
  const $0 = f();
  const $1 = g(1);
  return v2 => {
    if (v2.tag === "Nothing") { return $0; }
    if (v2.tag === "Just") { return $1(v2._1); }
    $runtime.fail();
  };
};
const test1 = f => {
  const $0 = f();
  return v2 => {
    if (v2.tag === "Nothing") { return $0; }
    if (v2.tag === "Just") { return 1 + v2._1 | 0; }
    $runtime.fail();
  };
};
export {test1, test2, test3, test4, test5};

// @inline Snapshot.BranchSpecialization01.eqTest.eq arity=2
import * as $runtime from "../runtime.js";
const $Test = tag => ({tag});
const Foo = /* #__PURE__ */ $Test("Foo");
const Bar = /* #__PURE__ */ $Test("Bar");
const Baz = /* #__PURE__ */ $Test("Baz");
const Qux = /* #__PURE__ */ $Test("Qux");
const eqTest = {
  eq: x => y => {
    if (x.tag === "Foo") { if (y.tag === "Foo") { return true; } }
    if (x.tag === "Bar") { if (y.tag === "Bar") { return true; } }
    if (x.tag === "Baz") { if (y.tag === "Baz") { return true; } }
    if (x.tag === "Qux") { if (y.tag === "Qux") { return true; } }
    return false;
  }
};
const test1 = a => a.tag === "Baz";
const test2 = a => a.tag === "Baz";
export {$Test, Bar, Baz, Foo, Qux, eqTest, test1, test2};

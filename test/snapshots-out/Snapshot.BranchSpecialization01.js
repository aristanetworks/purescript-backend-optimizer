// @inline Snapshot.BranchSpecialization01.eqTest.eq arity=2
import * as $runtime from "../runtime.js";
const $Test = tag => ({tag});
const Foo = /* #__PURE__ */ $Test("Foo");
const Bar = /* #__PURE__ */ $Test("Bar");
const Baz = /* #__PURE__ */ $Test("Baz");
const Qux = /* #__PURE__ */ $Test("Qux");
const eqTest = {
  eq: x => y => x.tag === "Foo" && y.tag === "Foo" || (x.tag === "Bar" && y.tag === "Bar" || (x.tag === "Baz" && y.tag === "Baz" || x.tag === "Qux" && y.tag === "Qux"))
};
const test1 = a => a.tag === "Baz";
const test2 = a => a.tag === "Baz";
export {$Test, Bar, Baz, Foo, Qux, eqTest, test1, test2};

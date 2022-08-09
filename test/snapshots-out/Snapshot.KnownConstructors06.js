// @inline Data.Show.Generic.genericShowConstructor arity=2
// @inline export genericTest.from arity=1
import * as $runtime from "../runtime.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
const $Test = tag => ({tag});
const Foo = /* #__PURE__ */ $Test("Foo");
const Bar = /* #__PURE__ */ $Test("Bar");
const Baz = /* #__PURE__ */ $Test("Baz");
const Qux = /* #__PURE__ */ $Test("Qux");
const genericTest = {
  to: x => {
    if (x.tag === "Inl") { return Foo; }
    if (x.tag === "Inr" && x._1.tag === "Inl") { return Bar; }
    if (x.tag === "Inr" && (x._1.tag === "Inr" && x._1._1.tag === "Inl")) { return Baz; }
    if (x.tag === "Inr" && (x._1.tag === "Inr" && x._1._1.tag === "Inr")) { return Qux; }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Foo") { return Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments); }
    if (x.tag === "Bar") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "Baz") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))); }
    if (x.tag === "Qux") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.NoArguments))); }
    $runtime.fail();
  }
};
const showTest = {
  show: x => {
    if (x.tag === "Foo") { return "Foo"; }
    if (x.tag === "Bar") { return "Bar"; }
    if (x.tag === "Baz") { return "Baz"; }
    if (x.tag === "Qux") { return "Qux"; }
    $runtime.fail();
  }
};
export {$Test, Bar, Baz, Foo, Qux, genericTest, showTest};

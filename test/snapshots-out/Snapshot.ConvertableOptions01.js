// @inline ConvertableOptions.convertRecordOptionsCons arity=6
// @inline ConvertableOptions.convertRecordOptionsNil always
// @inline export flub always
// @inline export flubImpl never
import * as $runtime from "../runtime.js";
import * as ConvertableOptions from "../ConvertableOptions/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Record$dBuilder from "../Record.Builder/index.js";
import * as Record$dUnsafe$dUnion from "../Record.Unsafe.Union/index.js";
const $Flub = () => ({tag: "Flub"});
const barIsSymbol = {reflectSymbol: () => "bar"};
const bazIsSymbol = {reflectSymbol: () => "baz"};
const fooIsSymbol = {reflectSymbol: () => "foo"};
const flubImpl = /* #__PURE__ */ (() => Data$dShow.showRecord()()(Data$dShow.showRecordFieldsCons(barIsSymbol)(Data$dShow.showRecordFieldsCons(bazIsSymbol)(Data$dShow.showRecordFieldsCons(fooIsSymbol)(Data$dShow.showRecordFieldsNil)(Data$dShow.showInt))({
  show: v => {
    if (v.tag === "Just") {
      if (v._1) { return "(Just true)"; }
      return "(Just false)";
    }
    if (v.tag === "Nothing") { return "Nothing"; }
    $runtime.fail();
  }
}))(Data$dShow.showString)).show)();
const defaultOptions = {foo: 42, baz: Data$dMaybe.Nothing};
const test1 = /* #__PURE__ */ flubImpl(/* #__PURE__ */ $runtime.recordUnionMutateLeft({bar: "Hello"}, defaultOptions));
const test2 = /* #__PURE__ */ flubImpl(/* #__PURE__ */ $runtime.recordUnionMutateLeft({foo: 99, bar: "Hello"}, defaultOptions));
const test3 = /* #__PURE__ */ flubImpl(/* #__PURE__ */ $runtime.recordUnionMutateLeft(
  {foo: 99, baz: /* #__PURE__ */ Data$dMaybe.$Maybe("Just", true), bar: "Hello"},
  defaultOptions
));
const test4 = /* #__PURE__ */ flubImpl(/* #__PURE__ */ $runtime.recordUnionMutateLeft(
  {foo: 99, baz: /* #__PURE__ */ Data$dMaybe.$Maybe("Just", true), bar: /* #__PURE__ */ Data$dShow.showIntImpl(42)},
  defaultOptions
));
export {$Flub, barIsSymbol, bazIsSymbol, defaultOptions, flubImpl, fooIsSymbol, test1, test2, test3, test4};

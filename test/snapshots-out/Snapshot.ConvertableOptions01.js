// @inline ConvertableOptions.convertRecordOptionsCons arity=6
// @inline ConvertableOptions.convertRecordOptionsNil always
// @inline export flub always
// @inline export flubImpl never
import * as $runtime from "./runtime.js";
import * as ConvertableOptions from "./ConvertableOptions.js";
import * as Data$dMaybe from "./Data.Maybe.js";
import * as Data$dShow from "./Data.Show.js";
import * as Record$dBuilder from "./Record.Builder.js";
import * as Record$dUnsafe$dUnion from "./Record.Unsafe.Union.js";
import * as Type$dProxy from "./Type.Proxy.js";
const $Flub = () => ({tag: "Flub"});
const barIsSymbol = /* #__PURE__ */ (() => ({reflectSymbol: () => "bar"}))();
const bazIsSymbol = /* #__PURE__ */ (() => ({reflectSymbol: () => "baz"}))();
const fooIsSymbol = /* #__PURE__ */ (() => ({reflectSymbol: () => "foo"}))();
const flubImpl = /* #__PURE__ */ (() => Data$dShow.showRecord()()(Data$dShow.showRecordFieldsCons(barIsSymbol)(Data$dShow.showRecordFieldsCons(bazIsSymbol)(Data$dShow.showRecordFieldsCons(fooIsSymbol)(Data$dShow.showRecordFieldsNil)(Data$dShow.showInt))({
  show: v => {
    if (v.tag === "Just") {
      return "(Just " + (
        (() => {
          if (v._1) { return "true"; }
          return "false";
        })() + ")"
      );
    }
    if (v.tag === "Nothing") { return "Nothing"; }
    throw new Error("Failed pattern match");
  }
}))(Data$dShow.showString)).show)();
const defaultOptions = /* #__PURE__ */ (() => ({foo: 42, baz: Data$dMaybe.Nothing}))();
const test1 = /* #__PURE__ */ flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({bar: "Hello"}, defaultOptions));
const test2 = /* #__PURE__ */ flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({foo: 99, bar: "Hello"}, defaultOptions));
const test3 = /* #__PURE__ */ flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({foo: 99, baz: Data$dMaybe.$Maybe("Just", true), bar: "Hello"}, defaultOptions));
const test4 = /* #__PURE__ */ flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({foo: 99, baz: Data$dMaybe.$Maybe("Just", true), bar: Data$dShow.showIntImpl(42)}, defaultOptions));
export {$Flub, barIsSymbol, bazIsSymbol, defaultOptions, flubImpl, fooIsSymbol, test1, test2, test3, test4};

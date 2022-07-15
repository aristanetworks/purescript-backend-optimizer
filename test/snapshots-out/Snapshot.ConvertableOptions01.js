import * as ConvertableOptions from "./ConvertableOptions.js";
import * as Data$dMaybe from "./Data.Maybe.js";
import * as Data$dShow from "./Data.Show.js";
import * as Record$dBuilder from "./Record.Builder.js";
import * as Record$dUnsafe$dUnion from "./Record.Unsafe.Union.js";
import * as Type$dProxy from "./Type.Proxy.js";
const $Flub = () => ({tag: "Flub"});
const barIsSymbol = {reflectSymbol: () => "bar"};
const bazIsSymbol = {reflectSymbol: () => "baz"};
const fooIsSymbol = {reflectSymbol: () => "foo"};
const flubImpl = Data$dShow.showRecord()()(Data$dShow.showRecordFieldsCons(barIsSymbol)(Data$dShow.showRecordFieldsCons(bazIsSymbol)(Data$dShow.showRecordFieldsCons(fooIsSymbol)(Data$dShow.showRecordFieldsNil)(Data$dShow.showInt))({
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
}))(Data$dShow.showString)).show;
const defaultOptions = {foo: 42, baz: Data$dMaybe.Nothing};
const test1 = flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({bar: "Hello"}, defaultOptions));
const test2 = flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({foo: 99, bar: "Hello"}, defaultOptions));
const test3 = flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({foo: 99, baz: Data$dMaybe.$Maybe("Just", true), bar: "Hello"}, defaultOptions));
const test4 = flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn({foo: 99, baz: Data$dMaybe.$Maybe("Just", true), bar: Data$dShow.showIntImpl(42)}, defaultOptions));
export {$Flub, barIsSymbol, bazIsSymbol, defaultOptions, flubImpl, fooIsSymbol, test1, test2, test3, test4};
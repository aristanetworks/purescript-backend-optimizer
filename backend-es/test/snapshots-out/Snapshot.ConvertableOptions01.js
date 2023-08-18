// @inline ConvertableOptions.convertRecordOptionsCons arity=6
// @inline ConvertableOptions.convertRecordOptionsNil always
// @inline export flub always
// @inline export flubImpl never
import * as ConvertableOptions from "../ConvertableOptions/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Record$dBuilder from "../Record.Builder/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Record$dUnsafe$dUnion from "../Record.Unsafe.Union/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const $Flub = () => ({tag: "Flub"});
const barIsSymbol = {reflectSymbol: () => "bar"};
const bazIsSymbol = {reflectSymbol: () => "baz"};
const Flub = /* #__PURE__ */ $Flub();
const flubImpl = v => "???";
const defaultOptions = {foo: 42, baz: Data$dMaybe.Nothing};
const convertRecordOptionsCons1 = /* #__PURE__ */ (() => {
  const $0 = {reflectSymbol: () => "foo"};
  return ConvertableOptions.convertRecordOptionsCons({
    convertRecordOptions: t => v => r => Record$dBuilder.insert()()($0)(Type$dProxy.Proxy)(Record$dUnsafe.unsafeGet($0.reflectSymbol(Type$dProxy.Proxy))(r))
  });
})();
const convertFlubBaz2 = {convertOption: v => v1 => mb => mb};
const convertFlubBaz1 = {convertOption: v => v1 => bool => Data$dMaybe.$Maybe("Just", bool)};
const convertFlubBar2 = {convertOption: v => v1 => str => str};
const test1 = /* #__PURE__ */ flubImpl({...defaultOptions, bar: "Hello"});
const test2 = /* #__PURE__ */ (() => flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn(
  convertRecordOptionsCons1(convertFlubBar2)()()()(barIsSymbol).convertRecordOptions(Flub)(Type$dProxy.Proxy)({foo: 99, bar: "Hello"})({}),
  defaultOptions
)))();
const test3 = /* #__PURE__ */ (() => {
  const $0 = {foo: 99, bar: "Hello", baz: Data$dMaybe.$Maybe("Just", true)};
  return flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn(
    Record$dBuilder.unsafeInsert("bar")($0.bar)(convertRecordOptionsCons1(convertFlubBaz2)()()()(bazIsSymbol).convertRecordOptions(Flub)(Type$dProxy.Proxy)($0)({})),
    defaultOptions
  ));
})();
const test4 = /* #__PURE__ */ (() => {
  const $0 = {foo: 99, bar: 42, baz: true};
  return flubImpl(Record$dUnsafe$dUnion.unsafeUnionFn(
    Record$dBuilder.unsafeInsert("bar")(Data$dShow.showIntImpl($0.bar))(convertRecordOptionsCons1(convertFlubBaz1)()()()(bazIsSymbol).convertRecordOptions(Flub)(Type$dProxy.Proxy)($0)({})),
    defaultOptions
  ));
})();
export {$Flub, Flub, barIsSymbol, bazIsSymbol, convertFlubBar2, convertFlubBaz1, convertFlubBaz2, convertRecordOptionsCons1, defaultOptions, flubImpl, test1, test2, test3, test4};

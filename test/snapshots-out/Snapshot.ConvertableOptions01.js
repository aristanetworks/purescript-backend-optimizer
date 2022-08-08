// @inline ConvertableOptions.convertRecordOptionsCons arity=6
// @inline ConvertableOptions.convertRecordOptionsNil always
// @inline export flub always
import * as $runtime from "../runtime.js";
import * as ConvertableOptions from "../ConvertableOptions/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Record$dBuilder from "../Record.Builder/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Record$dUnsafe$dUnion from "../Record.Unsafe.Union/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
import * as Snapshot$dConvertableOptions01$foreign from "./foreign.js";
const $Flub = () => ({tag: "Flub"});
const flubImpl = Snapshot$dConvertableOptions01$foreign.flubImpl;
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
export {$Flub, defaultOptions, flubImpl, test1, test2, test3, test4};
export * from "./foreign.js";

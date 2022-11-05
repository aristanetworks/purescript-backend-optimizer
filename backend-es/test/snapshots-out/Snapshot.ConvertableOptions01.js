// @inline ConvertableOptions.convertRecordOptionsCons arity=6
// @inline ConvertableOptions.convertRecordOptionsNil always
// @inline export flub always
import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import {flubImpl} from "./foreign.js";
const defaultOptions = {foo: 42, baz: Data$dMaybe.Nothing};
const test1 = /* #__PURE__ */ flubImpl({...defaultOptions, bar: "Hello"});
const test2 = /* #__PURE__ */ flubImpl({...defaultOptions, foo: 99, bar: "Hello"});
const test3 = /* #__PURE__ */ flubImpl({...defaultOptions, foo: 99, baz: /* #__PURE__ */ Data$dMaybe.$Maybe("Just", true), bar: "Hello"});
const test4 = /* #__PURE__ */ flubImpl({...defaultOptions, foo: 99, baz: /* #__PURE__ */ Data$dMaybe.$Maybe("Just", true), bar: "42"});
export {defaultOptions, test1, test2, test3, test4};
export * from "./foreign.js";

// @inline export  watUnit(..).wat1 arity=1
// @inline export  watUnit(..).wat2 arity=1
import * as Type$dEquality from "../Type.Equality/index.js";
import {testImpl} from "./foreign.js";
const watUnit = dictTypeEquals => {
  const to = dictTypeEquals.proof(a => a);
  return {wat1: x => testImpl(to(x)), wat2: x => testImpl(to(x))};
};
const watUnit1 = /* #__PURE__ */ watUnit(Type$dEquality.refl);
const wat2 = dict => dict.wat2;
const wat1 = dict => dict.wat1;
const g = /* #__PURE__ */ (() => watUnit1.wat2)();
const test2 = /* #__PURE__ */ testImpl();
const f = /* #__PURE__ */ (() => watUnit1.wat1)();
const test1 = /* #__PURE__ */ testImpl();
export {f, g, test1, test2, wat1, wat2, watUnit, watUnit1};
export * from "./foreign.js";

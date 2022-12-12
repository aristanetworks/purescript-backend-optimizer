// @inline export watUnit(..).wat arity=1
// @inline export testImpl never
import * as Type$dEquality from "../Type.Equality/index.js";
const wat = dict => dict.wat;
const testImpl = x => x;
const watUnit = dictTypeEquals => (
  {
    wat: (() => {
      const $0 = dictTypeEquals.proof(a => a);
      return x => testImpl($0(x));
    })()
  }
);
const wat1 = /* #__PURE__ */ (() => watUnit(Type$dEquality.refl).wat)();
const g = wat1;
const test2 = /* #__PURE__ */ testImpl();
const f = wat1;
const test1 = /* #__PURE__ */ testImpl();
export {f, g, test1, test2, testImpl, wat, wat1, watUnit};

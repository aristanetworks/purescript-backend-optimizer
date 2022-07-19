import * as $runtime from "./runtime.js";
import * as Control$dSemigroupoid from "./Control.Semigroupoid.js";
import * as Data$dSemiring from "./Data.Semiring.js";
import * as Data$dTuple from "./Data.Tuple.js";
import * as Data$dUnit from "./Data.Unit.js";
const State = x => x;
const functorState = /* #__PURE__ */ (() => (
  {
    map: f => v => next1 => s1 => v(s2 => {
      const _5 = next1(s2);
      return x => _5(f(x));
    })(s1)
  }
))();
const monadState = /* #__PURE__ */ (() => ({Applicative0: () => applicativeState, Bind1: () => bindState}))();
const bindState = /* #__PURE__ */ (() => ({bind: v => k2 => next1 => s1 => v(s2 => a => k2(a)(next1)(s2))(s1), Apply0: () => applyState}))();
const applyState = /* #__PURE__ */ (() => (
  {apply: f => a => next1 => s1 => f(s2 => a_1 => a(s2_1 => a_2 => applicativeState.pure(a_1(a_2))(next1)(s2_1))(s2))(s1), Functor0: () => functorState}
))();
const applicativeState = /* #__PURE__ */ (() => ({pure: a => next => s1 => next(s1)(a), Apply0: () => applyState}))();
const runState = s => v => v(Data$dTuple.Tuple)(s);
const put = s => next => v => next(s)(Data$dUnit.unit);
const $$get = next => s => next(s)(s);
const test4 = next1 => s1 => next1(s1 + 2 | 0)(Data$dUnit.unit);
export {State, applicativeState, applyState, bindState, functorState, $$get as get, monadState, put, runState, test4};

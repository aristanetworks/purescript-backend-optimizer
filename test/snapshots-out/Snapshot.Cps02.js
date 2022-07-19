// @inline Snapshot.Cps02.mkState always
// @inline Snapshot.Cps02.unState always
// @inline Snapshot.Cps02.put arity=1
// @inline Snapshot.Cps02.get always
import * as $runtime from "./runtime.js";
import * as Control$dSemigroupoid from "./Control.Semigroupoid.js";
import * as Data$dSemiring from "./Data.Semiring.js";
import * as Data$dTuple from "./Data.Tuple.js";
import * as Data$dUnit from "./Data.Unit.js";
const State = x => x;
const unState = v => k$p => s => v((_3, _4) => k$p(_3)(_4), s);
const runState = s => k => k((_2, _3) => Data$dTuple.$Tuple(_2, _3), s);
const mkState = k => (k$p, s) => k(_3 => _4 => k$p(_3, _4))(s);
const put = s => (k$p, s_1) => k$p(s, Data$dUnit.unit);
const functorState = /* #__PURE__ */ (() => ({map: f => k => (k$p, s) => k((_4, _5) => k$p(_4, f(_5)), s)}))();
const monadState = /* #__PURE__ */ (() => ({Applicative0: () => applicativeState, Bind1: () => bindState}))();
const bindState = /* #__PURE__ */ (() => ({bind: k1 => k2 => (k$p, s) => k1((_4, _5) => k2(_5)((_6, _7) => k$p(_6, _7), _4), s), Apply0: () => applyState}))();
const applyState = /* #__PURE__ */ (() => (
  {apply: f => a => (k$p, s) => f((_4, _5) => a((_6, _7) => applicativeState.pure(_5(_7))((_8, _9) => k$p(_8, _9), _6), _4), s), Functor0: () => functorState}
))();
const applicativeState = /* #__PURE__ */ (() => ({pure: a => (k$p, s) => k$p(s, a), Apply0: () => applyState}))();
const $$get = (k$p, s) => k$p(s, s);
const test4 = (k$p, s) => k$p(s + 2 | 0, Data$dUnit.unit);
export {State, applicativeState, applyState, bindState, functorState, $$get as get, mkState, monadState, put, runState, test4, unState};

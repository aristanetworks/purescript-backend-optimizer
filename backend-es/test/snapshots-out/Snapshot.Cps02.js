// @inline Snapshot.Cps02.mkState always
// @inline Snapshot.Cps02.unState always
// @inline Snapshot.Cps02.put arity=1
// @inline Snapshot.Cps02.get always
import * as Data$dTuple from "../Data.Tuple/index.js";
const State = x => x;
const unState = v => k$p => s => v(($3, $4) => k$p($3)($4), s);
const runState = s => k => k(($2, $3) => Data$dTuple.$Tuple($2, $3), s);
const mkState = k => (k$p, s) => k($3 => $4 => k$p($3, $4))(s);
const put = s => (k$p, s$1) => k$p(s, undefined);
const functorState = {map: f => k => (k$p, s) => k(($4, $5) => k$p($4, f($5)), s)};
const monadState = {Applicative0: () => applicativeState, Bind1: () => bindState};
const bindState = {bind: k1 => k2 => (k$p, s) => k1(($4, $5) => k2($5)(($6, $7) => k$p($6, $7), $4), s), Apply0: () => applyState};
const applyState = {apply: f => a => (k$p, s) => f(($4, $5) => a(($6, $7) => applicativeState.pure($5($7))(($8, $9) => k$p($8, $9), $6), $4), s), Functor0: () => functorState};
const applicativeState = {pure: a => (k$p, s) => k$p(s, a), Apply0: () => applyState};
const $$get = (k$p, s) => k$p(s, s);
const test4 = (k$p, s) => k$p(s + 2 | 0, undefined);
export {State, applicativeState, applyState, bindState, functorState, $$get as get, mkState, monadState, put, runState, test4, unState};

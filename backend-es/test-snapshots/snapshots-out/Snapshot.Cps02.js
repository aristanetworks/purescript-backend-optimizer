// @inline Snapshot.Cps02.mkState always
// @inline Snapshot.Cps02.unState always
// @inline Snapshot.Cps02.put arity=1
// @inline Snapshot.Cps02.get always
import * as Data$dTuple from "../Data.Tuple/index.js";
const State = x => x;
const unState = v => k$p => s => v(($0, $1) => k$p($0)($1), s);
const runState = s => k => k(($0, $1) => Data$dTuple.$Tuple($0, $1), s);
const mkState = k => (k$p, s) => k($0 => $1 => k$p($0, $1))(s);
const put = s => (k$p, s$1) => k$p(s, undefined);
const functorState = {map: f => k => (k$p, s) => k(($0, $1) => k$p($0, f($1)), s)};
const monadState = {Applicative0: () => applicativeState, Bind1: () => bindState};
const bindState = {bind: k1 => k2 => (k$p, s) => k1(($0, $1) => k2($1)(($2, $3) => k$p($2, $3), $0), s), Apply0: () => applyState};
const applyState = {apply: f => a => (k$p, s) => f(($0, $1) => a(($2, $3) => applicativeState.pure($1($3))(($4, $5) => k$p($4, $5), $2), $0), s), Functor0: () => functorState};
const applicativeState = {pure: a => (k$p, s) => k$p(s, a), Apply0: () => applyState};
const $$get = (k$p, s) => k$p(s, s);
const test4 = (k$p, s) => k$p(s + 2 | 0, undefined);
export {State, applicativeState, applyState, bindState, functorState, $$get as get, mkState, monadState, put, runState, test4, unState};

import * as Data$dTuple from "../Data.Tuple/index.js";
const State = x => x;
const functorState = {
  map: f => v => next1 => s1 => v(s2 => {
    const $5 = next1(s2);
    return x => $5(f(x));
  })(s1)
};
const monadState = {Applicative0: () => applicativeState, Bind1: () => bindState};
const bindState = {bind: v => k2 => next1 => s1 => v(s2 => a => k2(a)(next1)(s2))(s1), Apply0: () => applyState};
const applyState = {apply: f => a => next1 => s1 => f(s2 => a$1 => a(s2$1 => a$2 => applicativeState.pure(a$1(a$2))(next1)(s2$1))(s2))(s1), Functor0: () => functorState};
const applicativeState = {pure: a => next => s1 => next(s1)(a), Apply0: () => applyState};
const runState = s => v => v(Data$dTuple.Tuple)(s);
const put = s => next => v => next(s)();
const $$get = next => s => next(s)(s);
const test4 = next1 => s1 => next1(s1 + 2 | 0)();
export {State, applicativeState, applyState, bindState, functorState, $$get as get, monadState, put, runState, test4};

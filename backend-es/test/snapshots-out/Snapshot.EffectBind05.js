const Id = x => x;
const monadId = {Applicative0: () => applicativeId, Bind1: () => bindId};
const functorId = {map: f => a => bindId.bind(a)(a$p => applicativeId.pure(f(a$p)))};
const bindId = {bind: v => k => k(v()), Apply0: () => applyId};
const applyId = {apply: f => a => applicativeId.pure(f()(a())), Functor0: () => functorId};
const applicativeId = {pure: a => v => a, Apply0: () => applyId};
const test2 = k => k();
const test1 = k => k();
export {Id, applicativeId, applyId, bindId, functorId, monadId, test1, test2};

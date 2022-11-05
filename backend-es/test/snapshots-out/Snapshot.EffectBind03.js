import {random} from "./foreign.js";
const MyEffect = x => x;
const functorMyEffect = {
  map: f => v => () => {
    const a$p = v();
    return f(a$p);
  }
};
const monadMyEffect = {Applicative0: () => applicativeMyEffect, Bind1: () => bindMyEffect};
const bindMyEffect = {
  bind: v => k => () => {
    const a$p = v();
    return k(a$p)();
  },
  Apply0: () => applyMyEffect
};
const applyMyEffect = {
  apply: f => a => () => {
    const a$p = f();
    const a$p$1 = a();
    return applicativeMyEffect.pure(a$p(a$p$1))();
  },
  Functor0: () => functorMyEffect
};
const applicativeMyEffect = {pure: x => () => x, Apply0: () => applyMyEffect};
const test = () => {
  const a$p = random();
  const a$p$1 = random();
  return a$p + a$p$1 | 0;
};
export {MyEffect, applicativeMyEffect, applyMyEffect, bindMyEffect, functorMyEffect, monadMyEffect, test};
export * from "./foreign.js";

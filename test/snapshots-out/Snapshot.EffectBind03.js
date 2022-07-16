import * as Control$dMonad from "./Control.Monad.js";
import * as Data$dSemiring from "./Data.Semiring.js";
import * as Effect from "./Effect.js";
import * as Snapshot$dEffectBind03$foreign from "./Snapshot.EffectBind03.foreign.js";
const random = Snapshot$dEffectBind03$foreign.random;
const MyEffect = x => x;
const functorMyEffect = {
  map: f => v => () => {
    const a$p = v();
    return f(a$p);
  }
};
let monadMyEffect;
let bindMyEffect;
let applyMyEffect;
let applicativeMyEffect;
monadMyEffect = {Applicative0: () => applicativeMyEffect, Bind1: () => bindMyEffect};
bindMyEffect = {
  bind: v => k => () => {
    const a$p = v();
    return k(a$p)();
  },
  Apply0: () => applyMyEffect
};
applyMyEffect = {apply: Control$dMonad.ap(monadMyEffect), Functor0: () => functorMyEffect};
applicativeMyEffect = {pure: x => () => x, Apply0: () => applyMyEffect};
const test = () => {
  const a$p = random();
  const a$p_1 = random();
  return a$p + a$p_1 | 0;
};
export {MyEffect, applicativeMyEffect, applyMyEffect, bindMyEffect, functorMyEffect, monadMyEffect, random, test};
export * from "./Snapshot.EffectBind03.foreign.js";
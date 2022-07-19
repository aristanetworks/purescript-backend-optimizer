import * as $runtime from "./runtime.js";
import * as Data$dSemiring from "./Data.Semiring.js";
import * as Effect from "./Effect.js";
import * as Snapshot$dEffectBind03$foreign from "./Snapshot.EffectBind03.foreign.js";
const random = Snapshot$dEffectBind03$foreign.random;
const MyEffect = x => x;
const functorMyEffect = /* #__PURE__ */ (() => (
  {
    map: f => v => () => {
      const a$p = v();
      return f(a$p);
    }
  }
))();
const monadMyEffect = /* #__PURE__ */ (() => ({Applicative0: () => applicativeMyEffect, Bind1: () => bindMyEffect}))();
const bindMyEffect = /* #__PURE__ */ (() => (
  {
    bind: v => k => () => {
      const a$p = v();
      return k(a$p)();
    },
    Apply0: () => applyMyEffect
  }
))();
const applyMyEffect = /* #__PURE__ */ (() => (
  {
    apply: f => a => () => {
      const a$p = f();
      const a$p_1 = a();
      return applicativeMyEffect.pure(a$p(a$p_1))();
    },
    Functor0: () => functorMyEffect
  }
))();
const applicativeMyEffect = /* #__PURE__ */ (() => ({pure: x => () => x, Apply0: () => applyMyEffect}))();
const test = () => {
  const a$p = random();
  const a$p_1 = random();
  return a$p + a$p_1 | 0;
};
export {MyEffect, applicativeMyEffect, applyMyEffect, bindMyEffect, functorMyEffect, monadMyEffect, random, test};
export * from "./Snapshot.EffectBind03.foreign.js";

import * as $runtime from "./runtime.js";
import * as Data$dSemiring from "./Data.Semiring.js";
import * as Effect from "./Effect.js";
import * as Snapshot$dEffectBind02$foreign from "./Snapshot.EffectBind02.foreign.js";
const random = Snapshot$dEffectBind02$foreign.random;
const MyEffect = x => x;
const monadMyEffect = Effect.monadEffect;
const functorMyEffect = Effect.functorEffect;
const bindMyEffect = Effect.bindEffect;
const applyMyEffect = Effect.applyEffect;
const applicativeMyEffect = Effect.applicativeEffect;
const test = () => {
  const a = random();
  const b = random();
  return a + b | 0;
};
export {MyEffect, applicativeMyEffect, applyMyEffect, bindMyEffect, functorMyEffect, monadMyEffect, random, test};
export * from "./Snapshot.EffectBind02.foreign.js";

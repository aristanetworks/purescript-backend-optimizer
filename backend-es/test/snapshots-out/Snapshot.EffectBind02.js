import * as Effect from "../Effect/index.js";
import {random} from "./foreign.js";
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
export {MyEffect, applicativeMyEffect, applyMyEffect, bindMyEffect, functorMyEffect, monadMyEffect, test};
export * from "./foreign.js";

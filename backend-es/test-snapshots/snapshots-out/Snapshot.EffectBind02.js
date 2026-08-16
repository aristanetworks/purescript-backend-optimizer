import * as $mEffect from "../Effect/index.js";
const MyEffect = x => x;
const monadMyEffect = $mEffect.monadEffect;
const functorMyEffect = $mEffect.functorEffect;
const bindMyEffect = $mEffect.bindEffect;
const applyMyEffect = $mEffect.applyEffect;
const applicativeMyEffect = $mEffect.applicativeEffect;
const test = random => () => {
  const a = random();
  const b = random();
  return a + b | 0;
};
export {MyEffect, applicativeMyEffect, applyMyEffect, bindMyEffect, functorMyEffect, monadMyEffect, test};

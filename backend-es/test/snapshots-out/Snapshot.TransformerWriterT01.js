import * as $runtime from "../runtime.js";
import * as Control$dMonad$dWriter$dTrans from "../Control.Monad.Writer.Trans/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const monadEffectWriter = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadEffectWriter(Data$dMonoid.monoidString);
const monadTellWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadTellWriterT(Data$dMonoid.monoidString);
const applicativeWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.applicativeWriterT(Data$dMonoid.monoidString);
const monadTellWriterT1 = /* #__PURE__ */ monadTellWriterT(Effect.monadEffect);
const monadEffectWriter1 = /* #__PURE__ */ monadEffectWriter(Effect$dClass.monadEffectEffect);
const test4 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const bindWriterT2 = Control$dMonad$dWriter$dTrans.bindWriterT(Data$dSemigroup.semigroupString)(Bind1);
  const liftEffect1 = monadEffectWriter(dictMonadEffect).liftEffect;
  const Apply0 = Bind1.Apply0();
  const map2 = Control$dMonad$dWriter$dTrans.functorWriterT(Apply0.Functor0()).map;
  const apply1 = Control$dMonad$dWriter$dTrans.applyWriterT(Data$dSemigroup.semigroupString)(Apply0).apply;
  const tell1 = monadTellWriterT(Monad0).tell;
  const pure1 = applicativeWriterT(Monad0.Applicative0()).pure;
  return bindWriterT2.bind(liftEffect1(Effect$dConsole.log("foo")))(() => bindWriterT2.bind(liftEffect1(Effect$dRandom.randomInt(1)(10)))(i1 => bindWriterT2.bind(map2(v => v + 1 | 0)(liftEffect1(Effect$dRandom.randomInt(1)(10))))(i2 => bindWriterT2.bind(apply1(map2(Data$dSemiring.intAdd)(liftEffect1(Effect$dRandom.randomInt(1)(10))))(liftEffect1(Effect$dRandom.randomInt(1)(10))))(i3 => bindWriterT2.bind(tell1("nothing"))(() => pure1((
    (4 + i1 | 0) + i2 | 0
  ) + i3 | 0))))));
};
const test5 = /* #__PURE__ */ (() => {
  const $0 = test4(Effect$dClass.monadEffectEffect);
  return () => {
    const a$p = $0();
    return a$p._1;
  };
})();
const test2 = dictMonadTell => dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map2 = Apply0.Functor0().map;
  const pure1 = Monad0.Applicative0().pure;
  return Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map2(v => v + 1 | 0)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map2(Data$dSemiring.intAdd)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind(dictMonadTell.tell("nothing"))(() => pure1((
    (4 + i1 | 0) + i2 | 0
  ) + i3 | 0))))));
};
const test3 = /* #__PURE__ */ (() => {
  const $0 = test2(monadTellWriterT1)(monadEffectWriter1);
  return () => {
    const a$p = $0();
    return a$p._1;
  };
})();
const test1 = () => {
  Effect$dConsole.log("foo")();
  const a = Effect$dRandom.randomInt(1)(10)();
  const a$1 = Effect$dRandom.randomInt(1)(10)();
  const a$2 = Effect$dRandom.randomInt(1)(10)();
  const a$3 = Effect$dRandom.randomInt(1)(10)();
  return ((4 + a | 0) + (a$1 + 1 | 0) | 0) + (a$2 + a$3 | 0) | 0;
};
export {applicativeWriterT, monadEffectWriter, monadEffectWriter1, monadTellWriterT, monadTellWriterT1, test1, test2, test3, test4, test5};

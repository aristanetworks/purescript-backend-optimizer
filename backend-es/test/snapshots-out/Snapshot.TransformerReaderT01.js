// @inline export test2 always
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const monadReaderReaderT = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadReaderReaderT(Effect.monadEffect);
const monadEffectReader = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadEffectReader(Effect$dClass.monadEffectEffect);
const test4 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const bindReaderT1 = Control$dMonad$dReader$dTrans.bindReaderT(Bind1);
  const liftEffect1 = Control$dMonad$dReader$dTrans.monadEffectReader(dictMonadEffect).liftEffect;
  const Apply0 = Bind1.Apply0();
  const $6 = Apply0.Functor0();
  const $7 = Monad0.Applicative0().pure;
  const local1 = Control$dMonad$dReader$dTrans.monadReaderReaderT(Monad0).local;
  const $9 = Monad0.Applicative0();
  return bindReaderT1.bind(liftEffect1(Effect$dConsole.log("foo")))(() => bindReaderT1.bind(liftEffect1(Effect$dRandom.randomInt(1)(10)))(i1 => bindReaderT1.bind((() => {
    const $12 = $6.map(v => v + 1 | 0);
    const $13 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    return x => $12($13(x));
  })())(i2 => bindReaderT1.bind((() => {
    const $13 = $6.map(Data$dSemiring.intAdd);
    const $14 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    const $15 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    return r => Apply0.apply($13($14(r)))($15(r));
  })())(i3 => bindReaderT1.bind($7)(five => bindReaderT1.bind(local1(v => v * 2 | 0)(bindReaderT1.bind($7)(ten => liftEffect1(Effect$dRandom.randomInt(ten)(20)))))(i4 => {
    const $16 = $9.pure(((((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0) + i4 | 0);
    return v => $16;
  }))))));
};
const test5 = /* #__PURE__ */ test4(Effect$dClass.monadEffectEffect)(5);
const test2 = dictMonadReader => {
  const ask1 = dictMonadReader.MonadAsk0().ask;
  return dictMonadEffect => {
    const Monad0 = dictMonadEffect.Monad0();
    const Bind1 = Monad0.Bind1();
    const Apply0 = Bind1.Apply0();
    const map1 = Apply0.Functor0().map;
    const pure1 = Monad0.Applicative0().pure;
    return Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map1(v => v + 1 | 0)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map1(Data$dSemiring.intAdd)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind(ask1)(five => Bind1.bind(dictMonadReader.local(v => v * 2 | 0)(Bind1.bind(ask1)(ten => dictMonadEffect.liftEffect(Effect$dRandom.randomInt(ten)(20)))))(i4 => pure1((
      (((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0
    ) + i4 | 0)))))));
  };
};
const test3 = /* #__PURE__ */ (() => {
  const ask1 = monadReaderReaderT.MonadAsk0().ask;
  const Monad0 = monadEffectReader.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map1 = Apply0.Functor0().map;
  const pure1 = Monad0.Applicative0().pure;
  return Bind1.bind((() => {
    const $6 = Effect$dConsole.log("foo");
    return v => $6;
  })())(() => Bind1.bind((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return v => $7;
  })())(i1 => Bind1.bind(map1(v => v + 1 | 0)((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return v => $8;
  })()))(i2 => Bind1.bind(Apply0.apply(map1(Data$dSemiring.intAdd)((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return v => $9;
  })()))((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return v => $9;
  })()))(i3 => Bind1.bind(ask1)(five => Bind1.bind((() => {
    const $11 = Bind1.bind(ask1)(ten => {
      const $12 = Effect$dRandom.randomInt(ten)(20);
      return v => $12;
    });
    return x => $11(x * 2 | 0);
  })())(i4 => pure1(((((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0) + i4 | 0)))))))(5);
})();
const test1 = () => {
  Effect$dConsole.log("foo")();
  const a = Effect$dRandom.randomInt(1)(10)();
  const a$p = Effect$dRandom.randomInt(1)(10)();
  const a$p$1 = Effect$dRandom.randomInt(1)(10)();
  const a$p$2 = Effect$dRandom.randomInt(1)(10)();
  const a$1 = Effect$dRandom.randomInt(10)(20)();
  return ((((4 + a | 0) + (a$p + 1 | 0) | 0) + (a$p$1 + a$p$2 | 0) | 0) + 5 | 0) + a$1 | 0;
};
export {monadEffectReader, monadReaderReaderT, test1, test2, test3, test4, test5};

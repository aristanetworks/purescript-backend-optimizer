// @inline export program1 arity=1
// @inline export program2 arity=1
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const monadAskReaderT = /* #__PURE__ */ (() => {
  const monadReaderT1 = Control$dMonad$dReader$dTrans.monadReaderT(Effect.monadEffect);
  return {ask: Effect.pureE, Monad0: () => monadReaderT1};
})();
const test1 = () => {
  Effect$dConsole.log("foo")();
  const a = Effect$dRandom.randomInt(1)(10)();
  const a$p = Effect$dRandom.randomInt(1)(10)();
  const a$p$1 = Effect$dRandom.randomInt(1)(10)();
  const a$p$2 = Effect$dRandom.randomInt(1)(10)();
  const a$1 = monadAskReaderT.ask(5)();
  const a$2 = Effect$dRandom.randomInt(a$1)(20)();
  const a$3 = monadAskReaderT.ask(5)();
  return (((((4 + a | 0) + (a$p + 1 | 0) | 0) + (a$p$1 + a$p$2 | 0) | 0) + a$1 | 0) + a$2 | 0) + a$3 | 0;
};
const program2 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const bindReaderT1 = Control$dMonad$dReader$dTrans.bindReaderT(Bind1);
  const liftEffect1 = Control$dMonad$dReader$dTrans.monadEffectReader(dictMonadEffect).liftEffect;
  const Apply0 = Bind1.Apply0();
  const $6 = Apply0.Functor0();
  const $7 = Monad0.Applicative0().pure;
  const $8 = Monad0.Applicative0();
  return bindReaderT1.bind(liftEffect1(Effect$dConsole.log("foo")))(() => bindReaderT1.bind(liftEffect1(Effect$dRandom.randomInt(1)(10)))(i1 => bindReaderT1.bind((() => {
    const $11 = $6.map(v => v + 1 | 0);
    const $12 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    return x => $11($12(x));
  })())(i2 => bindReaderT1.bind((() => {
    const $12 = $6.map(Data$dSemiring.intAdd);
    const $13 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    const $14 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    return r => Apply0.apply($12($13(r)))($14(r));
  })())(i3 => bindReaderT1.bind($7)(five => bindReaderT1.bind(liftEffect1(Effect$dRandom.randomInt(five)(20)))(i4 => bindReaderT1.bind($7)(five2 => {
    const $16 = $8.pure((((((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0) + i4 | 0) + five2 | 0);
    return v => $16;
  })))))));
};
const test3 = /* #__PURE__ */ (() => {
  const bindReaderT1 = Control$dMonad$dReader$dTrans.bindReaderT(Effect.bindEffect);
  const liftEffect1 = Control$dMonad$dReader$dTrans.monadEffectReader(Effect$dClass.monadEffectEffect).liftEffect;
  return bindReaderT1.bind(liftEffect1(Effect$dConsole.log("foo")))(() => bindReaderT1.bind(liftEffect1(Effect$dRandom.randomInt(1)(10)))(i1 => bindReaderT1.bind((() => {
    const $4 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    return x => {
      const $6 = $4(x);
      return () => {
        const a$p = $6();
        return a$p + 1 | 0;
      };
    };
  })())(i2 => bindReaderT1.bind((() => {
    const $5 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    const $6 = liftEffect1(Effect$dRandom.randomInt(1)(10));
    return r => {
      const $8 = $5(r);
      const $9 = $6(r);
      return () => {
        const a$p = $8();
        const a$p$1 = $9();
        return a$p + a$p$1 | 0;
      };
    };
  })())(i3 => bindReaderT1.bind(Effect.pureE)(five => bindReaderT1.bind(liftEffect1(Effect$dRandom.randomInt(five)(20)))(i4 => bindReaderT1.bind(Effect.pureE)(five2 => {
    const $9 = (((((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0) + i4 | 0) + five2 | 0;
    return v => () => $9;
  })))))))(5);
})();
const program1 = dictMonadAsk => {
  const Monad0 = dictMonadAsk.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map1 = Apply0.Functor0().map;
  const pure1 = Monad0.Applicative0().pure;
  return dictMonadEffect => Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map1(v => v + 1 | 0)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map1(Data$dSemiring.intAdd)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind(dictMonadAsk.ask)(five => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(five)(20)))(i4 => Bind1.bind(dictMonadAsk.ask)(five2 => pure1((
    ((((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0) + i4 | 0
  ) + five2 | 0))))))));
};
const test2 = /* #__PURE__ */ (() => {
  const Monad0 = monadAskReaderT.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map1 = Apply0.Functor0().map;
  const pure1 = Monad0.Applicative0().pure;
  return Bind1.bind((() => {
    const $5 = Effect$dConsole.log("foo");
    return v => $5;
  })())(() => Bind1.bind((() => {
    const $6 = Effect$dRandom.randomInt(1)(10);
    return v => $6;
  })())(i1 => Bind1.bind(map1(v => v + 1 | 0)((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return v => $7;
  })()))(i2 => Bind1.bind(Apply0.apply(map1(Data$dSemiring.intAdd)((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return v => $8;
  })()))((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return v => $8;
  })()))(i3 => Bind1.bind(monadAskReaderT.ask)(five => Bind1.bind((() => {
    const $10 = Effect$dRandom.randomInt(five)(20);
    return v => $10;
  })())(i4 => Bind1.bind(monadAskReaderT.ask)(five2 => pure1((((((4 + i1 | 0) + i2 | 0) + i3 | 0) + five | 0) + i4 | 0) + five2 | 0))))))))(5);
})();
export {monadAskReaderT, program1, program2, test1, test2, test3};

// @inline export program1 arity=1
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dMaybe$dTrans from "../Control.Monad.Maybe.Trans/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const test1 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("foo");
  return () => {
    $0();
    const a$p = Effect$dRandom.randomInt(1)(10)();
    const a$p$1 = Effect$dRandom.randomInt(1)(10)();
    const $4 = a$p$1 + 5 | 0;
    const bind = Control$dMonad$dMaybe$dTrans.bindMaybeT(Effect.monadEffect).bind;
    const pure = Control$dMonad$dMaybe$dTrans.applicativeMaybeT(Effect.monadEffect).pure;
    const $7 = Effect$dRandom.randomInt(1)(10);
    const $8 = Effect$dRandom.randomInt(1)(10);
    const v1 = bind(() => {
      const a$p$2 = $7();
      return Data$dMaybe.$Maybe("Just", $10 => a$p$2 + $10 | 0);
    })(f$p => bind(() => {
      const a$p$2 = $8();
      return Data$dMaybe.$Maybe("Just", a$p$2);
    })(a$p$2 => pure(f$p(a$p$2))))();
    if (v1.tag === "Nothing") { return Data$dMaybe.Nothing; }
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", ((1 + a$p | 0) + $4 | 0) + v1._1 | 0); }
    $runtime.fail();
  };
})();
const program1 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const bindMaybeT1 = Control$dMonad$dMaybe$dTrans.bindMaybeT(Monad0);
  const Functor0 = Monad0.Bind1().Apply0().Functor0();
  const apply1 = Control$dMonad$dMaybe$dTrans.applyMaybeT(Monad0).apply;
  const pure1 = Control$dMonad$dMaybe$dTrans.applicativeMaybeT(Monad0).pure;
  return bindMaybeT1.bind(Functor0.map(Data$dMaybe.Just)(dictMonadEffect.liftEffect(Effect$dConsole.log("foo"))))(() => bindMaybeT1.bind(dictMonadEffect.liftEffect((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $7();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })()))(i1 => bindMaybeT1.bind(Functor0.map(v1 => {
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v1._1 + 4 | 0); }
    return Data$dMaybe.Nothing;
  })(Functor0.map(v1 => {
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v1._1 + 1 | 0); }
    return Data$dMaybe.Nothing;
  })(dictMonadEffect.liftEffect((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $8();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })()))))(i2 => bindMaybeT1.bind(apply1(Functor0.map(Data$dMaybe.functorMaybe.map(Data$dSemiring.intAdd))(dictMonadEffect.liftEffect((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $9();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })())))(dictMonadEffect.liftEffect((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $9();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })())))(i3 => pure1(((1 + i1 | 0) + i2 | 0) + i3 | 0)))));
};
const test2 = /* #__PURE__ */ (() => {
  const bindMaybeT1 = Control$dMonad$dMaybe$dTrans.bindMaybeT(Effect.monadEffect);
  const apply1 = Control$dMonad$dMaybe$dTrans.applyMaybeT(Effect.monadEffect).apply;
  const pure1 = Control$dMonad$dMaybe$dTrans.applicativeMaybeT(Effect.monadEffect).pure;
  return bindMaybeT1.bind((() => {
    const $3 = Effect$dConsole.log("foo");
    return () => {
      const a$p = $3();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })())(() => bindMaybeT1.bind((() => {
    const $4 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $4();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })())(i1 => bindMaybeT1.bind((() => {
    const $5 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $5();
      return Data$dMaybe.$Maybe("Just", a$p + 5 | 0);
    };
  })())(i2 => bindMaybeT1.bind(apply1((() => {
    const $6 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $6();
      return Data$dMaybe.$Maybe("Just", $8 => a$p + $8 | 0);
    };
  })())((() => {
    const $6 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $6();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })()))(i3 => pure1(((1 + i1 | 0) + i2 | 0) + i3 | 0)))));
})();
export {program1, test1, test2};

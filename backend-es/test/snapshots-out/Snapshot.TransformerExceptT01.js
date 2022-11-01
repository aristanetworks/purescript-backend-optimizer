// @inline export program1 arity=1
// @inline export program2 arity=2
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const map1 = f => a => () => {
  const a$p = a();
  return (() => {
    if (a$p.tag === "Left") { return Data$dEither.$Either("Left", a$p._1); }
    if (a$p.tag === "Right") { return Data$dEither.$Either("Right", f(a$p._1)); }
    $runtime.fail();
  })();
};
const test1 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("foo");
  return () => {
    $0();
    const a$p = Effect$dRandom.randomInt(1)(10)();
    const v2 = map1(v => v + 4 | 0)((() => {
      const $3 = Effect$dRandom.randomInt(1)(10);
      return () => {
        const a$p$1 = $3();
        return Data$dEither.$Either("Right", a$p$1 + 1 | 0);
      };
    })())();
    if (v2.tag === "Left") { return Data$dEither.$Either("Left", v2._1); }
    if (v2.tag === "Right") {
      const bind = Control$dMonad$dExcept$dTrans.bindExceptT(Effect.monadEffect).bind;
      const pure = Control$dMonad$dExcept$dTrans.applicativeExceptT(Effect.monadEffect).pure;
      const $6 = Effect$dRandom.randomInt(1)(10);
      const v2$1 = bind(map1(Data$dSemiring.intAdd)((() => {
        const $7 = Effect$dRandom.randomInt(1)(10);
        return () => {
          const a$p$1 = $7();
          return Data$dEither.$Either("Right", a$p$1);
        };
      })()))(f$p => bind(() => {
        const a$p$1 = $6();
        return Data$dEither.$Either("Right", a$p$1);
      })(a$p$1 => pure(f$p(a$p$1))))();
      if (v2$1.tag === "Left") { return Data$dEither.$Either("Left", v2$1._1); }
      if (v2$1.tag === "Right") {
        const v2$2 = (() => {
          if ((a$p + v2._1 | 0) < v2$1._1) { return () => Data$dEither.$Either("Left", "error"); }
          return () => Data$dEither.$Either("Right", Data$dUnit.unit);
        })()();
        if (v2$2.tag === "Left") { return Data$dEither.$Either("Left", v2$2._1); }
        if (v2$2.tag === "Right") { return Data$dEither.$Either("Right", ((1 + a$p | 0) + v2._1 | 0) + v2$1._1 | 0); }
        $runtime.fail();
      }
      $runtime.fail();
    }
    $runtime.fail();
  };
})();
const program2 = dictMonadThrow => dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map3 = Apply0.Functor0().map;
  const Applicative0 = Monad0.Applicative0();
  return Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map3(v => v + 4 | 0)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map3(Data$dSemiring.intAdd)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind((() => {
    const $11 = dictMonadThrow.throwError("error");
    if ((i1 + i2 | 0) < i3) { return $11; }
    return Applicative0.pure(Data$dUnit.unit);
  })())(() => Applicative0.pure(((1 + i1 | 0) + i2 | 0) + i3 | 0))))));
};
const test3 = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dExcept$dTrans.monadEffectExceptT(Effect$dClass.monadEffectEffect);
  const Monad0 = $0.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map3 = Apply0.Functor0().map;
  const Applicative0 = Monad0.Applicative0();
  return Bind1.bind($0.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind($0.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map3(v => v + 4 | 0)($0.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map3(Data$dSemiring.intAdd)($0.liftEffect(Effect$dRandom.randomInt(1)(10))))($0.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind((() => {
    if ((i1 + i2 | 0) < i3) { return () => Data$dEither.$Either("Left", "error"); }
    return Applicative0.pure(Data$dUnit.unit);
  })())(() => Applicative0.pure(((1 + i1 | 0) + i2 | 0) + i3 | 0))))));
})();
const program1 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const bindExceptT1 = Control$dMonad$dExcept$dTrans.bindExceptT(Monad0);
  const Functor0 = Monad0.Bind1().Apply0().Functor0();
  const apply1 = Control$dMonad$dExcept$dTrans.applyExceptT(Monad0).apply;
  const applicativeExceptT1 = Control$dMonad$dExcept$dTrans.applicativeExceptT(Monad0);
  const throwError1 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Monad0).throwError;
  return bindExceptT1.bind(Functor0.map(Data$dEither.Right)(dictMonadEffect.liftEffect(Effect$dConsole.log("foo"))))(() => bindExceptT1.bind(dictMonadEffect.liftEffect((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $8();
      return Data$dEither.$Either("Right", a$p);
    };
  })()))(i1 => bindExceptT1.bind(Functor0.map(m => {
    if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
    if (m.tag === "Right") { return Data$dEither.$Either("Right", m._1 + 4 | 0); }
    $runtime.fail();
  })(Functor0.map(m => {
    if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
    if (m.tag === "Right") { return Data$dEither.$Either("Right", m._1 + 1 | 0); }
    $runtime.fail();
  })(dictMonadEffect.liftEffect((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $9();
      return Data$dEither.$Either("Right", a$p);
    };
  })()))))(i2 => bindExceptT1.bind(apply1(Functor0.map(Data$dEither.functorEither.map(Data$dSemiring.intAdd))(dictMonadEffect.liftEffect((() => {
    const $10 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $10();
      return Data$dEither.$Either("Right", a$p);
    };
  })())))(dictMonadEffect.liftEffect((() => {
    const $10 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $10();
      return Data$dEither.$Either("Right", a$p);
    };
  })())))(i3 => bindExceptT1.bind((() => {
    const $11 = throwError1("error");
    if ((i1 + i2 | 0) < i3) { return $11; }
    return applicativeExceptT1.pure(Data$dUnit.unit);
  })())(() => applicativeExceptT1.pure(((1 + i1 | 0) + i2 | 0) + i3 | 0))))));
};
const test2 = /* #__PURE__ */ (() => {
  const bindExceptT1 = Control$dMonad$dExcept$dTrans.bindExceptT(Effect.monadEffect);
  const apply1 = Control$dMonad$dExcept$dTrans.applyExceptT(Effect.monadEffect).apply;
  const applicativeExceptT1 = Control$dMonad$dExcept$dTrans.applicativeExceptT(Effect.monadEffect);
  const throwError1 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Effect.monadEffect).throwError;
  return bindExceptT1.bind((() => {
    const $4 = Effect$dConsole.log("foo");
    return () => {
      const a$p = $4();
      return Data$dEither.$Either("Right", a$p);
    };
  })())(() => bindExceptT1.bind((() => {
    const $5 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $5();
      return Data$dEither.$Either("Right", a$p);
    };
  })())(i1 => bindExceptT1.bind((() => {
    const $6 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $6();
      return Data$dEither.$Either("Right", a$p + 5 | 0);
    };
  })())(i2 => bindExceptT1.bind(apply1((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $7();
      return Data$dEither.$Either("Right", $9 => a$p + $9 | 0);
    };
  })())((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $7();
      return Data$dEither.$Either("Right", a$p);
    };
  })()))(i3 => bindExceptT1.bind((() => {
    const $8 = throwError1("error");
    if ((i1 + i2 | 0) < i3) { return $8; }
    return applicativeExceptT1.pure(Data$dUnit.unit);
  })())(() => applicativeExceptT1.pure(((1 + i1 | 0) + i2 | 0) + i3 | 0))))));
})();
export {map1, program1, program2, test1, test2, test3};

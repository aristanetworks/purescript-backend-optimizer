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
const monadErrorExceptT = /* #__PURE__ */ Control$dMonad$dExcept$dTrans.monadErrorExceptT(Effect.monadEffect);
const test1 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("foo");
  return () => {
    $0();
    const a$p = Effect$dRandom.randomInt(1)(10)();
    const v2 = (() => {
      if (a$p < 5) { return () => Data$dEither.$Either("Left", "oh no!"); }
      return () => Data$dEither.$Either("Right", Data$dUnit.unit);
    })()();
    if (v2.tag === "Left") { return Data$dEither.$Either("Left", v2._1); }
    if (v2.tag === "Right") {
      const v2$1 = map1(v => v + 4 | 0)((() => {
        const $4 = Effect$dRandom.randomInt(1)(10);
        return () => {
          const a$p$1 = $4();
          return Data$dEither.$Either("Right", a$p$1 + 1 | 0);
        };
      })())();
      if (v2$1.tag === "Left") { return Data$dEither.$Either("Left", v2$1._1); }
      if (v2$1.tag === "Right") {
        const bind = Control$dMonad$dExcept$dTrans.bindExceptT(Effect.monadEffect).bind;
        const pure = Control$dMonad$dExcept$dTrans.applicativeExceptT(Effect.monadEffect).pure;
        const $7 = Effect$dRandom.randomInt(1)(10);
        const v2$2 = bind(map1(Data$dSemiring.intAdd)((() => {
          const $8 = Effect$dRandom.randomInt(1)(10);
          return () => {
            const a$p$1 = $8();
            return Data$dEither.$Either("Right", a$p$1);
          };
        })()))(f$p => bind(() => {
          const a$p$1 = $7();
          return Data$dEither.$Either("Right", a$p$1);
        })(a$p$1 => pure(f$p(a$p$1))))();
        if (v2$2.tag === "Left") { return Data$dEither.$Either("Left", v2$2._1); }
        if (v2$2.tag === "Right") {
          const a$p$1 = Effect$dRandom.randomInt(1)(10)();
          const v2$3 = (() => {
            if (a$p$1 < 5) { return () => Data$dEither.$Either("Left", "below 5"); }
            return () => Data$dEither.$Either("Right", Data$dUnit.unit);
          })()();
          const v2$4 = (() => {
            if (v2$3.tag === "Left") { return () => Data$dEither.$Either("Left", v2$3._1); }
            if (v2$3.tag === "Right") { return () => Data$dEither.$Either("Right", a$p$1); }
            $runtime.fail();
          })()();
          const v2$5 = (() => {
            if (v2$4.tag === "Left") {
              if (v2$1._1 < 5) { return () => Data$dEither.$Either("Right", 8); }
              return () => Data$dEither.$Either("Left", v2$4._1);
            }
            if (v2$4.tag === "Right") { return () => Data$dEither.$Either("Right", v2$4._1); }
            $runtime.fail();
          })()();
          if (v2$5.tag === "Left") { return Data$dEither.$Either("Left", v2$5._1); }
          if (v2$5.tag === "Right") { return Data$dEither.$Either("Right", (((1 + a$p | 0) + v2$1._1 | 0) + v2$2._1 | 0) + v2$5._1 | 0); }
          $runtime.fail();
        }
        $runtime.fail();
      }
      $runtime.fail();
    }
    $runtime.fail();
  };
})();
const program2 = dictMonadError => {
  const throwError1 = dictMonadError.MonadThrow0().throwError;
  return dictMonadEffect => {
    const Monad0 = dictMonadEffect.Monad0();
    const Bind1 = Monad0.Bind1();
    const Apply0 = Bind1.Apply0();
    const map3 = Apply0.Functor0().map;
    const Applicative0 = Monad0.Applicative0();
    return Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map3(v => v + 4 | 0)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map3(Data$dSemiring.intAdd)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind(dictMonadError.catchError(Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i5 => Bind1.bind((() => {
      const $13 = throwError1("below 5");
      if (i5 < 5) { return $13; }
      return Applicative0.pure(Data$dUnit.unit);
    })())(() => Applicative0.pure(i5))))(e => {
      if (i2 < 5) { return Applicative0.pure(8); }
      return throwError1(e);
    }))(i4 => Applicative0.pure((((1 + i1 | 0) + i2 | 0) + i3 | 0) + i4 | 0))))));
  };
};
const test3 = /* #__PURE__ */ (() => {
  const throwError1 = monadErrorExceptT.MonadThrow0().throwError;
  const $1 = Control$dMonad$dExcept$dTrans.monadEffectExceptT(Effect$dClass.monadEffectEffect);
  const Monad0 = $1.Monad0();
  const Bind1 = Monad0.Bind1();
  const Apply0 = Bind1.Apply0();
  const map3 = Apply0.Functor0().map;
  const Applicative0 = Monad0.Applicative0();
  return Bind1.bind($1.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind($1.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(map3(v => v + 4 | 0)($1.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(Apply0.apply(map3(Data$dSemiring.intAdd)($1.liftEffect(Effect$dRandom.randomInt(1)(10))))($1.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind((() => {
    const $11 = Bind1.bind($1.liftEffect(Effect$dRandom.randomInt(1)(10)))(i5 => Bind1.bind((() => {
      const $12 = throwError1("below 5");
      if (i5 < 5) { return $12; }
      return Applicative0.pure(Data$dUnit.unit);
    })())(() => Applicative0.pure(i5)));
    return () => {
      const v2 = $11();
      if (v2.tag === "Left") {
        if (i2 < 5) { return Applicative0.pure(8)(); }
        return throwError1(v2._1)();
      }
      if (v2.tag === "Right") { return Data$dEither.$Either("Right", v2._1); }
      $runtime.fail();
    };
  })())(i4 => Applicative0.pure((((1 + i1 | 0) + i2 | 0) + i3 | 0) + i4 | 0))))));
})();
const program1 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const bindExceptT1 = Control$dMonad$dExcept$dTrans.bindExceptT(Monad0);
  const Functor0 = Monad0.Bind1().Apply0().Functor0();
  const apply1 = Control$dMonad$dExcept$dTrans.applyExceptT(Monad0).apply;
  const catchError1 = Control$dMonad$dExcept$dTrans.monadErrorExceptT(Monad0).catchError;
  const applicativeExceptT1 = Control$dMonad$dExcept$dTrans.applicativeExceptT(Monad0);
  const throwError1 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Monad0).throwError;
  return bindExceptT1.bind(Functor0.map(Data$dEither.Right)(dictMonadEffect.liftEffect(Effect$dConsole.log("foo"))))(() => bindExceptT1.bind(dictMonadEffect.liftEffect((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $9();
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
    const $10 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $10();
      return Data$dEither.$Either("Right", a$p);
    };
  })()))))(i2 => bindExceptT1.bind(apply1(Functor0.map(Data$dEither.functorEither.map(Data$dSemiring.intAdd))(dictMonadEffect.liftEffect((() => {
    const $11 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $11();
      return Data$dEither.$Either("Right", a$p);
    };
  })())))(dictMonadEffect.liftEffect((() => {
    const $11 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $11();
      return Data$dEither.$Either("Right", a$p);
    };
  })())))(i3 => bindExceptT1.bind(catchError1(bindExceptT1.bind(dictMonadEffect.liftEffect((() => {
    const $12 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $12();
      return Data$dEither.$Either("Right", a$p);
    };
  })()))(i5 => bindExceptT1.bind((() => {
    const $13 = throwError1("below 5");
    if (i5 < 5) { return $13; }
    return applicativeExceptT1.pure(Data$dUnit.unit);
  })())(() => applicativeExceptT1.pure(i5))))(e => {
    if (i2 < 5) { return applicativeExceptT1.pure(8); }
    return throwError1(e);
  }))(i4 => applicativeExceptT1.pure((((1 + i1 | 0) + i2 | 0) + i3 | 0) + i4 | 0))))));
};
const test2 = /* #__PURE__ */ (() => {
  const bindExceptT1 = Control$dMonad$dExcept$dTrans.bindExceptT(Effect.monadEffect);
  const apply1 = Control$dMonad$dExcept$dTrans.applyExceptT(Effect.monadEffect).apply;
  const catchError1 = Control$dMonad$dExcept$dTrans.monadErrorExceptT(Effect.monadEffect).catchError;
  const applicativeExceptT1 = Control$dMonad$dExcept$dTrans.applicativeExceptT(Effect.monadEffect);
  const throwError1 = Control$dMonad$dExcept$dTrans.monadThrowExceptT(Effect.monadEffect).throwError;
  return bindExceptT1.bind((() => {
    const $5 = Effect$dConsole.log("foo");
    return () => {
      const a$p = $5();
      return Data$dEither.$Either("Right", a$p);
    };
  })())(() => bindExceptT1.bind((() => {
    const $6 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $6();
      return Data$dEither.$Either("Right", a$p);
    };
  })())(i1 => bindExceptT1.bind((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $7();
      return Data$dEither.$Either("Right", a$p + 5 | 0);
    };
  })())(i2 => bindExceptT1.bind(apply1((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $8();
      return Data$dEither.$Either("Right", $10 => a$p + $10 | 0);
    };
  })())((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $8();
      return Data$dEither.$Either("Right", a$p);
    };
  })()))(i3 => bindExceptT1.bind(catchError1(bindExceptT1.bind((() => {
    const $9 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $9();
      return Data$dEither.$Either("Right", a$p);
    };
  })())(i5 => bindExceptT1.bind((() => {
    const $10 = throwError1("below 5");
    if (i5 < 5) { return $10; }
    return applicativeExceptT1.pure(Data$dUnit.unit);
  })())(() => applicativeExceptT1.pure(i5))))(e => {
    if (i2 < 5) { return applicativeExceptT1.pure(8); }
    return throwError1(e);
  }))(i4 => applicativeExceptT1.pure((((1 + i1 | 0) + i2 | 0) + i3 | 0) + i4 | 0))))));
})();
export {map1, monadErrorExceptT, program1, program2, test1, test2, test3};

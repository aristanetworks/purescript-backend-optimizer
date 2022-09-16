import * as $runtime from "../runtime.js";
import * as Control$dMonad$dMaybe$dTrans from "../Control.Monad.Maybe.Trans/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const bindMaybeT = /* #__PURE__ */ Control$dMonad$dMaybe$dTrans.bindMaybeT(Effect.monadEffect);
const pure = /* #__PURE__ */ (() => Control$dMonad$dMaybe$dTrans.applicativeMaybeT(Effect.monadEffect).pure)();
const test1 = /* #__PURE__ */ (() => bindMaybeT.bind((() => {
  const $0 = Effect$dConsole.log("foo");
  return () => {
    const a$p = $0();
    return Data$dMaybe.$Maybe("Just", a$p);
  };
})())(() => bindMaybeT.bind((() => {
  const $1 = Effect$dRandom.randomInt(1)(10);
  return () => {
    const a$p = $1();
    return Data$dMaybe.$Maybe("Just", a$p);
  };
})())(i1 => bindMaybeT.bind((() => {
  const $2 = Effect$dRandom.randomInt(1)(10);
  return () => {
    const a$p = $2();
    return Data$dMaybe.$Maybe("Just", a$p + 1 | 0);
  };
})())(i2 => bindMaybeT.bind((() => {
  const $3 = Effect$dRandom.randomInt(1)(10);
  const $4 = Effect$dRandom.randomInt(1)(10);
  return () => {
    const a$p = $3();
    const a$p$1 = $4();
    return Data$dMaybe.applyMaybe.apply(Data$dMaybe.$Maybe("Just", $7 => a$p + $7 | 0))(Data$dMaybe.$Maybe("Just", a$p$1));
  };
})())(i3 => pure(((1 + i1 | 0) + i2 | 0) + i3 | 0))))))();
const program1 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const bindMaybeT1 = Control$dMonad$dMaybe$dTrans.bindMaybeT(Monad0);
  const Apply0 = Monad0.Bind1().Apply0();
  const map2 = Apply0.Functor0().map;
  const pure1 = Control$dMonad$dMaybe$dTrans.applicativeMaybeT(Monad0).pure;
  return bindMaybeT1.bind(map2(Data$dMaybe.Just)(dictMonadEffect.liftEffect(Effect$dConsole.log("foo"))))(() => bindMaybeT1.bind(dictMonadEffect.liftEffect((() => {
    const $7 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $7();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })()))(i1 => bindMaybeT1.bind(map2(v1 => {
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", v1._1 + 1 | 0); }
    return Data$dMaybe.Nothing;
  })(dictMonadEffect.liftEffect((() => {
    const $8 = Effect$dRandom.randomInt(1)(10);
    return () => {
      const a$p = $8();
      return Data$dMaybe.$Maybe("Just", a$p);
    };
  })())))(i2 => bindMaybeT1.bind(Apply0.apply(map2(a => b => Data$dMaybe.applyMaybe.apply((() => {
    if (a.tag === "Just") { return Data$dMaybe.$Maybe("Just", $11 => a._1 + $11 | 0); }
    return Data$dMaybe.Nothing;
  })())(b))(dictMonadEffect.liftEffect((() => {
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
const test2 = /* #__PURE__ */ program1(Effect$dClass.monadEffectEffect);
export {bindMaybeT, program1, pure, test1, test2};

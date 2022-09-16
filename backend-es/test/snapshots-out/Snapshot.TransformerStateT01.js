import * as $runtime from "../runtime.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const monadEffectState = /* #__PURE__ */ Control$dMonad$dState$dTrans.monadEffectState(Effect$dClass.monadEffectEffect);
const monadStateStateT = /* #__PURE__ */ Control$dMonad$dState$dTrans.monadStateStateT(Effect.monadEffect);
const test1 = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dState$dTrans.monadTransStateT.lift(Effect.monadEffect)(Effect$dConsole.log("foo"))(1);
  return () => {
    const v1 = $0();
    const v1$1 = Control$dMonad$dState$dTrans.monadTransStateT.lift(Effect.monadEffect)(Effect$dRandom.randomInt(1)(10))(v1._2)();
    const a$p = Control$dMonad$dState$dTrans.monadTransStateT.lift(Effect.monadEffect)(Effect$dRandom.randomInt(1)(10))(v1$1._2)();
    const bind = Control$dMonad$dState$dTrans.bindStateT(Effect.monadEffect).bind;
    const pure = Control$dMonad$dState$dTrans.applicativeStateT(Effect.monadEffect).pure;
    const $6 = monadEffectState.liftEffect(Effect$dRandom.randomInt(1)(10));
    const $7 = monadEffectState.liftEffect(Effect$dRandom.randomInt(1)(10));
    const v1$2 = bind(s => {
      const $9 = $6(s);
      return () => {
        const a$p$1 = $9();
        return Data$dTuple.$Tuple($11 => a$p$1._1 + $11 | 0, a$p$1._2);
      };
    })(f$p => bind($7)(a$p$1 => pure(f$p(a$p$1))))(a$p._1 + v1$1._2 | 0)();
    const s$p = v1$2._2 + v1$2._1 | 0;
    return Data$dTuple.$Tuple(Data$dShow.showIntImpl(v1$1._1 + s$p | 0), s$p);
  };
})();
const program2 = dictMonadState => {
  const get1 = dictMonadState.state(s => Data$dTuple.$Tuple(s, s));
  return dictMonadEffect => {
    const Monad0 = dictMonadEffect.Monad0();
    const Bind1 = Monad0.Bind1();
    const Apply0 = Bind1.Apply0();
    const map1 = Apply0.Functor0().map;
    const pure1 = Monad0.Applicative0().pure;
    return Bind1.bind(dictMonadEffect.liftEffect(Effect$dConsole.log("foo")))(() => Bind1.bind(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10)))(i1 => Bind1.bind(get1)(one => Bind1.bind(map1(v => v + one | 0)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i2 => Bind1.bind(dictMonadState.state(v => Data$dTuple.$Tuple(
      Data$dUnit.unit,
      i2
    )))(() => Bind1.bind(Apply0.apply(map1(Data$dSemiring.intAdd)(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(dictMonadEffect.liftEffect(Effect$dRandom.randomInt(1)(10))))(i3 => Bind1.bind(dictMonadState.state(s => {
      const s$p = s + i3 | 0;
      return Data$dTuple.$Tuple(s$p, s$p);
    }))(result => pure1(Data$dShow.showIntImpl(i1 + result | 0)))))))));
  };
};
const test3 = /* #__PURE__ */ program2(monadStateStateT)(monadEffectState)(2);
const program1 = dictMonadEffect => {
  const Monad0 = dictMonadEffect.Monad0();
  const bindStateT1 = Control$dMonad$dState$dTrans.bindStateT(Monad0);
  const liftEffect1 = Control$dMonad$dState$dTrans.monadEffectState(dictMonadEffect).liftEffect;
  const monadStateStateT1 = Control$dMonad$dState$dTrans.monadStateStateT(Monad0);
  const get1 = monadStateStateT1.state(s => Data$dTuple.$Tuple(s, s));
  const map1 = Control$dMonad$dState$dTrans.functorStateT(Monad0.Bind1().Apply0().Functor0()).map;
  const apply1 = Control$dMonad$dState$dTrans.applyStateT(Monad0).apply;
  const pure1 = Control$dMonad$dState$dTrans.applicativeStateT(Monad0).pure;
  return bindStateT1.bind(liftEffect1(Effect$dConsole.log("foo")))(() => bindStateT1.bind(liftEffect1(Effect$dRandom.randomInt(1)(10)))(i1 => bindStateT1.bind(get1)(one => bindStateT1.bind(map1(v => v + one | 0)(liftEffect1(Effect$dRandom.randomInt(1)(10))))(i2 => bindStateT1.bind(monadStateStateT1.state(v => Data$dTuple.$Tuple(
    Data$dUnit.unit,
    i2
  )))(() => bindStateT1.bind(apply1(map1(Data$dSemiring.intAdd)(liftEffect1(Effect$dRandom.randomInt(1)(10))))(liftEffect1(Effect$dRandom.randomInt(1)(10))))(i3 => bindStateT1.bind(monadStateStateT1.state(s => {
    const s$p = s + i3 | 0;
    return Data$dTuple.$Tuple(s$p, s$p);
  }))(result => pure1(Data$dShow.showIntImpl(i1 + result | 0)))))))));
};
const test2 = /* #__PURE__ */ program1(Effect$dClass.monadEffectEffect)(1);
export {monadEffectState, monadStateStateT, program1, program2, test1, test2, test3};

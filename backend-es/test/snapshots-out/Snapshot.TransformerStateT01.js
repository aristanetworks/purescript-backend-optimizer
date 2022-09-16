// @inline export program1 arity=1
// @inline export program2 arity=2
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dState$dTrans from "../Control.Monad.State.Trans/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const liftEffect = /* #__PURE__ */ (() => Control$dMonad$dState$dTrans.monadEffectState(Effect$dClass.monadEffectEffect).liftEffect)();
const test1 = /* #__PURE__ */ (() => {
  const $0 = Control$dMonad$dState$dTrans.monadTransStateT.lift(Effect.monadEffect)(Effect$dConsole.log("foo"))(1);
  return () => {
    const v1 = $0();
    const v1$1 = Control$dMonad$dState$dTrans.monadTransStateT.lift(Effect.monadEffect)(Effect$dRandom.randomInt(1)(10))(v1._2)();
    const a$p = Control$dMonad$dState$dTrans.monadTransStateT.lift(Effect.monadEffect)(Effect$dRandom.randomInt(1)(10))(v1$1._2)();
    const bind = Control$dMonad$dState$dTrans.bindStateT(Effect.monadEffect).bind;
    const pure = Control$dMonad$dState$dTrans.applicativeStateT(Effect.monadEffect).pure;
    const $6 = liftEffect(Effect$dRandom.randomInt(1)(10));
    const $7 = liftEffect(Effect$dRandom.randomInt(1)(10));
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
export {liftEffect, test1};

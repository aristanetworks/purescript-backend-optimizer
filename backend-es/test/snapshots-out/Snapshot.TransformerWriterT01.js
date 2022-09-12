import * as $runtime from "../runtime.js";
import * as Control$dMonad$dWriter$dTrans from "../Control.Monad.Writer.Trans/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dRandom from "../Effect.Random/index.js";
const liftEffect = /* #__PURE__ */ (() => Control$dMonad$dWriter$dTrans.monadEffectWriter(Data$dMonoid.monoidString)(Effect$dClass.monadEffectEffect).liftEffect)();
const tell = /* #__PURE__ */ (() => Control$dMonad$dWriter$dTrans.monadTellWriterT(Data$dMonoid.monoidString)(Effect.monadEffect).tell)();
const pure = /* #__PURE__ */ (() => Control$dMonad$dWriter$dTrans.applicativeWriterT(Data$dMonoid.monoidString)(Effect.applicativeEffect).pure)();
const test1 = /* #__PURE__ */ (() => {
  const $0 = liftEffect(Effect$dConsole.log("foo"));
  return () => {
    $0();
    const a$p = (() => {
      const $2 = liftEffect(Effect$dRandom.randomInt(1)(10));
      return () => {
        const v1 = $2();
        const a$p = (() => {
          const $4 = liftEffect(Effect$dRandom.randomInt(1)(10));
          return () => {
            const a$p = $4();
            const $6 = a$p._1 + 1 | 0;
            const a$p$1 = (() => {
              const $7 = liftEffect(Effect$dRandom.randomInt(1)(10));
              const $8 = liftEffect(Effect$dRandom.randomInt(1)(10));
              return () => {
                const a$p$1 = $7();
                const a$p$2 = $8();
                const $11 = a$p$1._1 + a$p$2._1 | 0;
                const a$p$3 = (() => {
                  const $12 = tell("nothing");
                  return () => {
                    const v1$1 = $12();
                    const a$p$3 = pure(((4 + v1._1 | 0) + $6 | 0) + $11 | 0)();
                    return Data$dTuple.$Tuple(a$p$3._1, v1$1._2 + a$p$3._2);
                  };
                })()();
                return Data$dTuple.$Tuple(a$p$3._1, a$p$1._2 + a$p$2._2 + a$p$3._2);
              };
            })()();
            return Data$dTuple.$Tuple(a$p$1._1, a$p._2 + a$p$1._2);
          };
        })()();
        return Data$dTuple.$Tuple(a$p._1, v1._2 + a$p._2);
      };
    })()();
    return a$p._1;
  };
})();
export {liftEffect, pure, tell, test1};

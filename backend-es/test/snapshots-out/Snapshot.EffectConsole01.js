import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Control$dMonad$dWriter$dTrans from "../Control.Monad.Writer.Trans/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const discard1 = /* #__PURE__ */ (() => Control$dMonad$dWriter$dTrans.bindWriterT(Data$dSemigroup.semigroupUnit)(Effect.bindEffect).bind)();
const monadEffectWriter = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadEffectWriter(Data$dMonoid.monoidUnit)(Effect$dClass.monadEffectEffect);
const clear = /* #__PURE__ */ (() => monadEffectWriter.liftEffect(Effect$dConsole.clear))();
const discard2 = /* #__PURE__ */ (() => Control$dMonad$dReader$dTrans.bindReaderT(Effect.bindEffect).bind)();
const monadEffectReader = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadEffectReader(Effect$dClass.monadEffectEffect);
const clear1 = /* #__PURE__ */ (() => monadEffectReader.liftEffect(Effect$dConsole.clear))();
const test3 = /* #__PURE__ */ (() => {
  const $0 = discard1(monadEffectWriter.liftEffect(Effect$dConsole.log("hello")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.log("1")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.warn("hello")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.warn("1")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.error("hello")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.error("1")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.info("hello")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.info("1")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.debug("hello")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.debug("1")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.time("start")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.timeLog("middle")))(() => discard1(monadEffectWriter.liftEffect(Effect$dConsole.timeEnd("finish")))(() => clear)))))))))))));
  return () => {
    const a$p = $0();
    return a$p._2;
  };
})();
const test2 = /* #__PURE__ */ (() => discard2(monadEffectReader.liftEffect(Effect$dConsole.log("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.log("1")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.warn("1")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.error("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.error("1")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.info("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.info("1")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.debug("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.debug("1")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.time("start")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.timeLog("middle")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.timeEnd("finish")))(() => clear1)))))))))))))("input"))();
const test1 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("hello");
  return () => {
    $0();
    Effect$dConsole.log("1")();
    Effect$dConsole.warn("hello")();
    Effect$dConsole.warn("1")();
    Effect$dConsole.error("hello")();
    Effect$dConsole.error("1")();
    Effect$dConsole.info("hello")();
    Effect$dConsole.info("1")();
    Effect$dConsole.debug("hello")();
    Effect$dConsole.debug("1")();
    Effect$dConsole.time("start")();
    Effect$dConsole.timeLog("middle")();
    Effect$dConsole.timeEnd("finish")();
    return Effect$dConsole.clear();
  };
})();
export {clear, clear1, discard1, discard2, monadEffectReader, monadEffectWriter, test1, test2, test3};

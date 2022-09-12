// @inline export bindCalls always
// @inline export discardCalls always
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dWriter$dTrans from "../Control.Monad.Writer.Trans/index.js";
import * as Data$dMonoid from "../Data.Monoid/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const bindWriterT = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.bindWriterT(Data$dSemigroup.semigroupUnit)(Effect.bindEffect);
const monadEffectWriter = /* #__PURE__ */ Control$dMonad$dWriter$dTrans.monadEffectWriter(Data$dMonoid.monoidUnit)(Effect$dClass.monadEffectEffect);
const clear = /* #__PURE__ */ (() => monadEffectWriter.liftEffect(Effect$dConsole.clear))();
const test4 = /* #__PURE__ */ (() => {
  const $0 = bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.log("hello")))(() => bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.log("1")))(() => bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.warn("hello")))(() => bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.warn("1")))(() => clear))));
  return () => {
    const a$p = $0();
    return a$p._2;
  };
})();
const test3 = /* #__PURE__ */ (() => {
  const $0 = bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.log("hello")))(() => bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.log("1")))(() => bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.warn("hello")))(() => bindWriterT.bind(monadEffectWriter.liftEffect(Effect$dConsole.warn("1")))(() => clear))));
  return () => {
    const a$p = $0();
    return a$p._2;
  };
})();
const discardCalls = dictMonadEffect => {
  const discard2 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test2 = /* #__PURE__ */ (() => {
  const discard2 = monadEffectWriter.Monad0().Bind1().bind;
  const clear1 = monadEffectWriter.liftEffect(Effect$dConsole.clear);
  const $2 = discard2(monadEffectWriter.liftEffect(Effect$dConsole.log("hello")))(() => discard2(monadEffectWriter.liftEffect(Effect$dConsole.log("1")))(() => discard2(monadEffectWriter.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(monadEffectWriter.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
  return () => {
    const a$p = $2();
    return a$p._2;
  };
})();
const bindCalls = dictMonadEffect => {
  const bind1 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test1 = /* #__PURE__ */ (() => {
  const bind1 = monadEffectWriter.Monad0().Bind1().bind;
  const clear1 = monadEffectWriter.liftEffect(Effect$dConsole.clear);
  const $2 = bind1(monadEffectWriter.liftEffect(Effect$dConsole.log("hello")))(() => bind1(monadEffectWriter.liftEffect(Effect$dConsole.log("1")))(() => bind1(monadEffectWriter.liftEffect(Effect$dConsole.warn("hello")))(() => bind1(monadEffectWriter.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
  return () => {
    const a$p = $2();
    return a$p._2;
  };
})();
export {bindCalls, bindWriterT, clear, discardCalls, monadEffectWriter, test1, test2, test3, test4};

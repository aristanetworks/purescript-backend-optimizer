// @inline export bindCalls always
// @inline export discardCalls always
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const bindReaderT = /* #__PURE__ */ Control$dMonad$dReader$dTrans.bindReaderT(Effect.bindEffect);
const monadEffectReader = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadEffectReader(Effect$dClass.monadEffectEffect);
const clear = /* #__PURE__ */ (() => monadEffectReader.liftEffect(Effect$dConsole.clear))();
const test4 = /* #__PURE__ */ (() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.log("hello")))(() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.log("1")))(() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.warn("hello")))(() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.warn("1")))(() => clear))))("input"))();
const test3 = /* #__PURE__ */ (() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.log("hello")))(() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.log("1")))(() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.warn("hello")))(() => bindReaderT.bind(monadEffectReader.liftEffect(Effect$dConsole.warn("1")))(() => clear))))("input"))();
const discardCalls = dictMonadEffect => {
  const discard2 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test2 = /* #__PURE__ */ (() => {
  const discard2 = monadEffectReader.Monad0().Bind1().bind;
  const clear1 = monadEffectReader.liftEffect(Effect$dConsole.clear);
  return discard2(monadEffectReader.liftEffect(Effect$dConsole.log("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.log("1")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(monadEffectReader.liftEffect(Effect$dConsole.warn("1")))(() => clear1))))("input");
})();
const bindCalls = dictMonadEffect => {
  const bind1 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test1 = /* #__PURE__ */ (() => {
  const bind1 = monadEffectReader.Monad0().Bind1().bind;
  const clear1 = monadEffectReader.liftEffect(Effect$dConsole.clear);
  return bind1(monadEffectReader.liftEffect(Effect$dConsole.log("hello")))(() => bind1(monadEffectReader.liftEffect(Effect$dConsole.log("1")))(() => bind1(monadEffectReader.liftEffect(Effect$dConsole.warn("hello")))(() => bind1(monadEffectReader.liftEffect(Effect$dConsole.warn("1")))(() => clear1))))("input");
})();
export {bindCalls, bindReaderT, clear, discardCalls, monadEffectReader, test1, test2, test3, test4};

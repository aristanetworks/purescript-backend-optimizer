// @inline export bindCalls always
// @inline export discardCalls always
import * as $runtime from "../runtime.js";
import * as Control$dMonad$dReader$dTrans from "../Control.Monad.Reader.Trans/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const monadEffectReader = /* #__PURE__ */ Control$dMonad$dReader$dTrans.monadEffectReader(Effect$dClass.monadEffectEffect);
const test4 = () => {
  Effect$dConsole.log("hello")();
  Effect$dConsole.log("1")();
  Effect$dConsole.warn("hello")();
  Effect$dConsole.warn("1")();
  return Effect$dConsole.clear();
};
const test3 = () => {
  Effect$dConsole.log("hello")();
  Effect$dConsole.log("1")();
  Effect$dConsole.warn("hello")();
  Effect$dConsole.warn("1")();
  return Effect$dConsole.clear();
};
const discardCalls = dictMonadEffect => {
  const discard2 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test2 = /* #__PURE__ */ (() => {
  const discard2 = monadEffectReader.Monad0().Bind1().bind;
  return discard2((() => {
    const $1 = Effect$dConsole.log("hello");
    return v => $1;
  })())(() => discard2((() => {
    const $2 = Effect$dConsole.log("1");
    return v => $2;
  })())(() => discard2((() => {
    const $3 = Effect$dConsole.warn("hello");
    return v => $3;
  })())(() => discard2((() => {
    const $4 = Effect$dConsole.warn("1");
    return v => $4;
  })())(() => v => Effect$dConsole.clear))))("input");
})();
const bindCalls = dictMonadEffect => {
  const bind1 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test1 = /* #__PURE__ */ (() => {
  const bind1 = monadEffectReader.Monad0().Bind1().bind;
  return bind1((() => {
    const $1 = Effect$dConsole.log("hello");
    return v => $1;
  })())(() => bind1((() => {
    const $2 = Effect$dConsole.log("1");
    return v => $2;
  })())(() => bind1((() => {
    const $3 = Effect$dConsole.warn("hello");
    return v => $3;
  })())(() => bind1((() => {
    const $4 = Effect$dConsole.warn("1");
    return v => $4;
  })())(() => v => Effect$dConsole.clear))))("input");
})();
export {bindCalls, discardCalls, monadEffectReader, test1, test2, test3, test4};

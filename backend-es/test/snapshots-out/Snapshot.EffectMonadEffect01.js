// @inline export bindCalls always
// @inline export discardCalls always
import * as $runtime from "../runtime.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test4 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("hello");
  return () => {
    $0();
    Effect$dConsole.log("1")();
    Effect$dConsole.warn("hello")();
    Effect$dConsole.warn("1")();
    return Effect$dConsole.clear();
  };
})();
const test3 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("hello");
  return () => {
    $0();
    Effect$dConsole.log("1")();
    Effect$dConsole.warn("hello")();
    Effect$dConsole.warn("1")();
    return Effect$dConsole.clear();
  };
})();
const discardCalls = dictMonadEffect => {
  const discard2 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => discard2(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test2 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("hello");
  return () => {
    $0();
    Effect$dConsole.log("1")();
    Effect$dConsole.warn("hello")();
    Effect$dConsole.warn("1")();
    return Effect$dConsole.clear();
  };
})();
const bindCalls = dictMonadEffect => {
  const bind1 = dictMonadEffect.Monad0().Bind1().bind;
  const clear1 = dictMonadEffect.liftEffect(Effect$dConsole.clear);
  return bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.log("1")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("hello")))(() => bind1(dictMonadEffect.liftEffect(Effect$dConsole.warn("1")))(() => clear1))));
};
const test1 = /* #__PURE__ */ (() => {
  const $0 = Effect$dConsole.log("hello");
  return () => {
    $0();
    Effect$dConsole.log("1")();
    Effect$dConsole.warn("hello")();
    Effect$dConsole.warn("1")();
    return Effect$dConsole.clear();
  };
})();
export {bindCalls, discardCalls, test1, test2, test3, test4};

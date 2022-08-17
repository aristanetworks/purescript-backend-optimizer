// @inline export traverseFun1 arity=1
// @inline export rewriteBottomUpM arity=1
import * as $runtime from "../runtime.js";
const $Fun = (tag, _1, _2) => ({tag, _1, _2});
const Abs = value0 => value1 => $Fun("Abs", value0, value1);
const App = value0 => value1 => $Fun("App", value0, value1);
const traverseFun1 = dictApplicative => {
  const Apply0 = dictApplicative.Apply0();
  const map = Apply0.Functor0().map;
  return k => v => {
    if (v.tag === "Abs") { return map(Abs(v._1))(k(v._2)); }
    if (v.tag === "App") { return Apply0.apply(map(App)(k(v._1)))(k(v._2)); }
    $runtime.fail();
  };
};
const rewriteBottomUpM = dictMonad => {
  const Apply0 = dictMonad.Applicative0().Apply0();
  const map = Apply0.Functor0().map;
  const _3 = dictMonad.Bind1();
  return k => {
    const go = a => {
      if (a.tag === "Abs") { return map(Abs(a._1))(_3.bind(go(a._2))(k)); }
      if (a.tag === "App") { return Apply0.apply(map(App)(_3.bind(go(a._1))(k)))(_3.bind(go(a._2))(k)); }
      $runtime.fail();
    };
    return a => _3.bind(go(a))(k);
  };
};
const rewriteBottomUp = k => {
  const go = a => {
    if (a.tag === "Abs") { return $Fun("Abs", a._1, k(go(a._2))); }
    if (a.tag === "App") { return $Fun("App", k(go(a._1)), k(go(a._2))); }
    $runtime.fail();
  };
  return a => k(go(a));
};
export {$Fun, Abs, App, rewriteBottomUp, rewriteBottomUpM, traverseFun1};

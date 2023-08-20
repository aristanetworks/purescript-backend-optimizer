// @inline Snapshot.RecursionSchemes01.cata arity=2
// @inline Snapshot.RecursionSchemes01.functorExprF.map arity=2
import * as $runtime from "../runtime.js";
const $ExprF = (tag, _1, _2) => ({tag, _1, _2});
const Add = value0 => value1 => $ExprF("Add", value0, value1);
const Mul = value0 => value1 => $ExprF("Mul", value0, value1);
const Lit = value0 => $ExprF("Lit", value0);
const functorExprF = {
  map: f => m => {
    if (m.tag === "Add") { return $ExprF("Add", f(m._1), f(m._2)); }
    if (m.tag === "Mul") { return $ExprF("Mul", f(m._1), f(m._2)); }
    if (m.tag === "Lit") { return $ExprF("Lit", m._1); }
    $runtime.fail();
  }
};
const test1 = v => {
  if (v.tag === "Add") { return test1(v._1) + test1(v._2) | 0; }
  if (v.tag === "Mul") { return test1(v._1) * test1(v._2) | 0; }
  if (v.tag === "Lit") { return v._1; }
  $runtime.fail();
};
const test2 = v => {
  if (v.tag === "Add") { return test2(v._1) + test2(v._2) | 0; }
  if (v.tag === "Mul") { return test2(v._1) * test2(v._2) | 0; }
  if (v.tag === "Lit") { return v._1 + 1 | 0; }
  $runtime.fail();
};
export {$ExprF, Add, Lit, Mul, functorExprF, test1, test2};

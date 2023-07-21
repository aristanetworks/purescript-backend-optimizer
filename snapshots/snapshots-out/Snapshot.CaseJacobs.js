import * as $runtime from "../runtime.js";
const $Expr = (tag, _1, _2) => ({tag, _1, _2});
const Add = value0 => value1 => $Expr("Add", value0, value1);
const Mul = value0 => value1 => $Expr("Mul", value0, value1);
const Succ = value0 => $Expr("Succ", value0);
const Zero = /* #__PURE__ */ $Expr("Zero");
const showExpr = {
  show: v => {
    if (v.tag === "Add") { return "Add(" + showExpr.show(v._1) + " " + showExpr.show(v._2) + ")"; }
    if (v.tag === "Mul") { return "Mul(" + showExpr.show(v._1) + " " + showExpr.show(v._2) + ")"; }
    if (v.tag === "Succ") { return "Succ(" + showExpr.show(v._1) + ")"; }
    if (v.tag === "Zero") { return "Zero"; }
    $runtime.fail();
  }
};
const test1 = v => {
  if (v.tag === "Add") {
    if (v._2.tag === "Zero") {
      if (v._1.tag === "Zero") { return "e1"; }
      if (v._1.tag === "Succ") { return "e3: " + showExpr.show(v._1._1) + " " + showExpr.show(v._2); }
      return "e6: " + showExpr.show(v._1);
    }
    if (v._1.tag === "Succ") { return "e3: " + showExpr.show(v._1._1) + " " + showExpr.show(v._2); }
    return "e7: " + showExpr.show(v);
  }
  if (v.tag === "Mul") {
    if (v._1.tag === "Zero") { return "e2: " + showExpr.show(v._2); }
    if (v._2.tag === "Zero") { return "e4: " + showExpr.show(v._1); }
    if (v._1.tag === "Add") { return "e5: " + showExpr.show(v._1._1) + " " + showExpr.show(v._1._2) + " " + showExpr.show(v._2); }
    return "e7: " + showExpr.show(v);
  }
  return "e7: " + showExpr.show(v);
};
export {$Expr, Add, Mul, Succ, Zero, showExpr, test1};

import * as $runtime from "../runtime.js";
const $Product3 = (_1, _2, _3) => ({tag: "Product3", _1, _2, _3});
const Product3 = value0 => value1 => value2 => $Product3(value0, value1, value2);
const test1 = v => {
  if (v._1 === 1) {
    if (v._3 === 3) {
      if (v._2 === 2) { return "1"; }
      if (v._2 === 4) { return "2"; }
      return "catch";
    }
    if (v._2 === 4) { return "2"; }
    return "catch";
  }
  if (v._2 === 4) { return "2"; }
  if (v._1 === 4) {
    if (v._2 === 5) {
      if (v._3 === 6) { return "3"; }
      return "catch";
    }
    return "catch";
  }
  return "catch";
};
export {$Product3, Product3, test1};

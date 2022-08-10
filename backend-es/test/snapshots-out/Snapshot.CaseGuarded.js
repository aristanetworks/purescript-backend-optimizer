import * as $runtime from "../runtime.js";
import * as Data$dShow from "../Data.Show/index.js";
const $Product3 = (_1, _2, _3) => ({tag: "Product3", _1, _2, _3});
const Product3 = value0 => value1 => value2 => $Product3(value0, value1, value2);
const NInt = x => x;
const test5 = v => {
  if (v.tag === "Just") {
    if (v._1.tag === "Right") { return v._1._1; }
    if (v.tag === "Just") {
      if (v._1.tag === "Left") {
        if (v._1._1 === 2) { return 4; }
        return 5;
      }
      return 5;
    }
    return 5;
  }
  if (v.tag === "Just") {
    if (v._1.tag === "Left") {
      if (v._1._1 === 2) { return 4; }
      return 5;
    }
    return 5;
  }
  return 5;
};
const test4 = v => v1 => {
  if (v.a === 1) {
    if (v1.d === 1) { return 1; }
    if (v1.d === 2) { return 2; }
    if (v1.d === 3) { return 3; }
    if (v1.d === 4) { return 4; }
    if (v1.d === 5) { return 5; }
    return (11 + v.c | 0) + v1.f | 0;
  }
  if (v1.d === 2) { return 2; }
  if (v1.d === 3) { return 3; }
  if (v.a === 2) {
    if (v1.d === 1) { return 6; }
    if (v1.d === 4) {
      if (v.c === v1.e) { return 7; }
      if (v.c < v1.e) { return 8; }
      if (v.c > v1.e) { return 9; }
      return (11 + v.c | 0) + v1.f | 0;
    }
    return (11 + v.c | 0) + v1.f | 0;
  }
  if (v1.d === 4) {
    if (v.c === v1.e) { return 7; }
    if (v.c < v1.e) { return 8; }
    if (v.c > v1.e) { return 9; }
    return (11 + v.c | 0) + v1.f | 0;
  }
  if (v.b === 2) {
    if (v1.d === 1) {
      if (v1.f === 10) { return 10; }
      return (11 + v.c | 0) + v1.f | 0;
    }
    return (11 + v.c | 0) + v1.f | 0;
  }
  return (11 + v.c | 0) + v1.f | 0;
};
const test3 = v => {
  if (v._1 === v._2) { return v._1; }
  if (v._3 === v._2) { return v._1; }
  if (v._1 === v._3) { return v._3; }
  return v._2;
};
const test2 = v => {
  if (v < 1) { return v; }
  if (v > 1) { return v; }
  if (v === 1) { return 1; }
  return 0;
};
const test1 = v => {
  if (v < 1) { return "n: " + Data$dShow.showIntImpl(v); }
  if (v > 1) {
    if (v < 100) { return "1 < x < 100: " + Data$dShow.showIntImpl(v); }
    if (v > 1) {
      if (v < 50) { return "1 < x < 50: " + Data$dShow.showIntImpl(v); }
      return "catch";
    }
    return "catch";
  }
  if (v > 1) {
    if (v < 50) { return "1 < x < 50: " + Data$dShow.showIntImpl(v); }
    return "catch";
  }
  return "catch";
};
export {$Product3, NInt, Product3, test1, test2, test3, test4, test5};

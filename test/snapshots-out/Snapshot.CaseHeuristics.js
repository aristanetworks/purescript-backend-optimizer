import * as $runtime from "../runtime.js";
const $Column = (tag, _1, _2) => ({tag, _1, _2});
const Zero = /* #__PURE__ */ $Column("Zero");
const One = value0 => $Column("One", value0);
const Two = value0 => value1 => $Column("Two", value0, value1);
const testPBAN = v => v1 => {
  if (v.tag === "One") {
    if (v1.tag === "One") {
      if (v._1 === 1) {
        if (v1._1 === 1) { return 1; }
        return 4;
      }
      if (v._1 === 2) {
        if (v1._1 === 2) { return 2; }
        return 4;
      }
      return 4;
    }
    return 4;
  }
  if (v.tag === "Two") {
    if (v1.tag === "Two") { return 3; }
    return 4;
  }
  return 4;
};
const testPBA = v => v1 => {
  if (v1.tag === "One") {
    if (v.tag === "One") {
      if (v._1 === 1) {
        if (v1._1 === 1) { return 1; }
        return 4;
      }
      if (v._1 === 2) {
        if (v1._1 === 2) { return 2; }
        return 4;
      }
      return 4;
    }
    return 4;
  }
  if (v.tag === "Two") {
    if (v._1 === 1) {
      if (v1.tag === "Two") { return 3; }
      return 4;
    }
    return 4;
  }
  return 4;
};
const testPB = v => v1 => {
  if (v.tag === "One") {
    if (v._1 === 1) {
      if (v1.tag === "One") {
        if (v1._1 === 1) { return 1; }
        return 4;
      }
      if (v1.tag === "Zero") { return 3; }
      return 4;
    }
    if (v1.tag === "Zero") { return 3; }
    return 4;
  }
  if (v.tag === "Two") {
    if (v._1 === 2) {
      if (v._2 === 3) {
        if (v1.tag === "Two") {
          if (v1._1 === 2) {
            if (v1._2 === 3) { return 2; }
            return 4;
          }
          return 4;
        }
        if (v1.tag === "Zero") { return 3; }
        return 4;
      }
      if (v1.tag === "Zero") { return 3; }
      return 4;
    }
    if (v1.tag === "Zero") { return 3; }
    return 4;
  }
  if (v1.tag === "Zero") { return 3; }
  return 4;
};
const testP = v => v1 => v2 => {
  if (v1 === 2) {
    if (v === 1) {
      if (v2 === 1) { return 1; }
      if (v2 === 2) { return 2; }
      if (v2 === 3) { return 3; }
      if (v2 === 4) { return 4; }
      return 5;
    }
    if (v2 === 3) { return 3; }
    return 5;
  }
  if (v === 1) {
    if (v2 === 4) { return 4; }
    return 5;
  }
  return 5;
};
export {$Column, One, Two, Zero, testP, testPB, testPBA, testPBAN};

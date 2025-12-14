import * as $runtime from "../runtime.js";
const $Color = tag => tag;
const $RedBlackTree = (tag, _1, _2, _3, _4) => ({tag, _1, _2, _3, _4});
const Red = /* #__PURE__ */ $Color("Red");
const Black = /* #__PURE__ */ $Color("Black");
const Leaf = /* #__PURE__ */ $RedBlackTree("Leaf");
const Node = value0 => value1 => value2 => value3 => $RedBlackTree("Node", value0, value1, value2, value3);
const test1 = () => v => {
  if (v.tag === "Node" && v._1 === "Black") {
    if (v._2.tag === "Node") {
      if (v._2._1 === "Red") {
        if (v._2._2.tag === "Node") {
          if (v._2._2._1 === "Red") { return {i: 1, a: v._2._2._2, x: v._2._2._3, b: v._2._2._4, y: v._2._3, c: v._2._4, z: v._3, d: v._4}; }
          if (v._2._4.tag === "Node") {
            if (v._2._4._1 === "Red") { return {i: 2, a: v._2._2, x: v._2._3, b: v._2._4._2, y: v._2._4._3, c: v._2._4._4, z: v._3, d: v._4}; }
            if (v._4.tag === "Node" && v._4._1 === "Red") {
              if (v._4._2.tag === "Node" && v._4._2._1 === "Red") { return {i: 3, a: v._2, x: v._3, b: v._4._2._2, y: v._4._2._3, c: v._4._2._4, z: v._4._3, d: v._4._4}; }
              if (v._4._4.tag === "Node" && v._4._4._1 === "Red") { return {i: 4, a: v._2, x: v._3, b: v._4._2, y: v._4._3, c: v._4._4._2, z: v._4._4._3, d: v._4._4._4}; }
            }
            $runtime.fail();
          }
          if (v._4.tag === "Node" && v._4._1 === "Red") {
            if (v._4._2.tag === "Node" && v._4._2._1 === "Red") { return {i: 3, a: v._2, x: v._3, b: v._4._2._2, y: v._4._2._3, c: v._4._2._4, z: v._4._3, d: v._4._4}; }
            if (v._4._4.tag === "Node" && v._4._4._1 === "Red") { return {i: 4, a: v._2, x: v._3, b: v._4._2, y: v._4._3, c: v._4._4._2, z: v._4._4._3, d: v._4._4._4}; }
          }
          $runtime.fail();
        }
        if (v._2._4.tag === "Node") {
          if (v._2._4._1 === "Red") { return {i: 2, a: v._2._2, x: v._2._3, b: v._2._4._2, y: v._2._4._3, c: v._2._4._4, z: v._3, d: v._4}; }
          if (v._4.tag === "Node" && v._4._1 === "Red") {
            if (v._4._2.tag === "Node" && v._4._2._1 === "Red") { return {i: 3, a: v._2, x: v._3, b: v._4._2._2, y: v._4._2._3, c: v._4._2._4, z: v._4._3, d: v._4._4}; }
            if (v._4._4.tag === "Node" && v._4._4._1 === "Red") { return {i: 4, a: v._2, x: v._3, b: v._4._2, y: v._4._3, c: v._4._4._2, z: v._4._4._3, d: v._4._4._4}; }
          }
          $runtime.fail();
        }
        if (v._4.tag === "Node" && v._4._1 === "Red") {
          if (v._4._2.tag === "Node" && v._4._2._1 === "Red") { return {i: 3, a: v._2, x: v._3, b: v._4._2._2, y: v._4._2._3, c: v._4._2._4, z: v._4._3, d: v._4._4}; }
          if (v._4._4.tag === "Node" && v._4._4._1 === "Red") { return {i: 4, a: v._2, x: v._3, b: v._4._2, y: v._4._3, c: v._4._4._2, z: v._4._4._3, d: v._4._4._4}; }
        }
        $runtime.fail();
      }
      if (v._4.tag === "Node" && v._4._1 === "Red") {
        if (v._4._2.tag === "Node" && v._4._2._1 === "Red") { return {i: 3, a: v._2, x: v._3, b: v._4._2._2, y: v._4._2._3, c: v._4._2._4, z: v._4._3, d: v._4._4}; }
        if (v._4._4.tag === "Node" && v._4._4._1 === "Red") { return {i: 4, a: v._2, x: v._3, b: v._4._2, y: v._4._3, c: v._4._4._2, z: v._4._4._3, d: v._4._4._4}; }
      }
      $runtime.fail();
    }
    if (v._4.tag === "Node" && v._4._1 === "Red") {
      if (v._4._2.tag === "Node" && v._4._2._1 === "Red") { return {i: 3, a: v._2, x: v._3, b: v._4._2._2, y: v._4._2._3, c: v._4._2._4, z: v._4._3, d: v._4._4}; }
      if (v._4._4.tag === "Node" && v._4._4._1 === "Red") { return {i: 4, a: v._2, x: v._3, b: v._4._2, y: v._4._3, c: v._4._4._2, z: v._4._4._3, d: v._4._4._4}; }
    }
  }
  $runtime.fail();
};
export {$Color, $RedBlackTree, Black, Leaf, Node, Red, test1};

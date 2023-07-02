// @inline Snapshot.RecursionInlined07.foldlArray always
// @inline Snapshot.RecursionInlined07.foldlArray2 always
// @inline Snapshot.RecursionInlined07.nutsToHtml always
// Another "real-world" example from deku
import * as $runtime from "../runtime.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const $AttributeValue = (tag, _1) => ({tag, _1});
const $Child = (tag, _1) => ({tag, _1});
const $Entity = (tag, _1) => ({tag, _1});
const $Scope = (tag, _1) => ({tag, _1});
const Local = value0 => $Scope("Local", value0);
const Global = /* #__PURE__ */ $Scope("Global");
const Event = x => x;
const Element = x => x;
const DOMInterpret = x => x;
const Node = x => x;
const Insert = value0 => $Child("Insert", value0);
const Remove = /* #__PURE__ */ $Child("Remove");
const Logic = value0 => $Child("Logic", value0);
const DynamicChildren$p = value0 => $Entity("DynamicChildren'", value0);
const FixedChildren$p = value0 => $Entity("FixedChildren'", value0);
const EventfulElement$p = value0 => $Entity("EventfulElement'", value0);
const Element$p = value0 => $Entity("Element'", value0);
const DynamicChildren = x => x;
const FixedChildren = x => x;
const EventfulElement = x => x;
const NutF = x => x;
const Korok = x => x;
const Cb = x => x;
const Prop$p = value0 => $AttributeValue("Prop'", value0);
const Cb$p = value0 => $AttributeValue("Cb'", value0);
const Unset$p = /* #__PURE__ */ $AttributeValue("Unset'");
const VolatileAttribute = x => x;
const ActualizedKorok = x => x;
const PureKorok = x => x;
const Nut = x => x;
const foldlArray2 = foldlArray2$a0$copy => foldlArray2$a1$copy => foldlArray2$a2$copy => foldlArray2$a3$copy => foldlArray2$a4$copy => {
  let foldlArray2$a0 = foldlArray2$a0$copy;
  let foldlArray2$a1 = foldlArray2$a1$copy;
  let foldlArray2$a2 = foldlArray2$a2$copy;
  let foldlArray2$a3 = foldlArray2$a3$copy;
  let foldlArray2$a4 = foldlArray2$a4$copy;
  let foldlArray2$c = true;
  let foldlArray2$r;
  while (foldlArray2$c) {
    const n = foldlArray2$a0, i = foldlArray2$a1, bab = foldlArray2$a2, b = foldlArray2$a3, arr = foldlArray2$a4;
    if (n === i) {
      foldlArray2$c = false;
      foldlArray2$r = b;
      continue;
    }
    foldlArray2$a0 = n + 1 | 0;
    foldlArray2$a1 = i;
    foldlArray2$a2 = bab;
    foldlArray2$a3 = bab(b)(arr[n]);
    foldlArray2$a4 = arr;
  }
  return foldlArray2$r;
};
const foldlArray = bab => b => arr => foldlArray2(0)(arr.length)(bab)(b)(arr);
const delimiter = "qrs";
const nutsToHtml = actualized => count => foldlArray2(0)(actualized.length)(v1 => {
  const $0 = v1._1;
  const $1 = v1._2;
  return v2 => {
    if (v2.tag === "Left") {
      if (v2._1.tag === "Left") { return Data$dTuple.$Tuple($0 + v2._1._1, $1); }
      if (v2._1.tag === "Right") { return Data$dTuple.$Tuple($0 + v2._1._1.html, $1); }
      $runtime.fail();
    }
    if (v2.tag === "Right") { return Data$dTuple.$Tuple($0 + "qrs" + Data$dShow.showIntImpl($1) + "qrs", $1 + 1 | 0); }
    $runtime.fail();
  };
})(Data$dTuple.$Tuple("", count))(actualized)._1;
const test1 = "<div></div><h1></h1><b></b><i></i>";
export {
  $AttributeValue,
  $Child,
  $Entity,
  $Scope,
  ActualizedKorok,
  Cb,
  Cb$p,
  DOMInterpret,
  DynamicChildren,
  DynamicChildren$p,
  Element,
  Element$p,
  Event,
  EventfulElement,
  EventfulElement$p,
  FixedChildren,
  FixedChildren$p,
  Global,
  Insert,
  Korok,
  Local,
  Logic,
  Node,
  Nut,
  NutF,
  Prop$p,
  PureKorok,
  Remove,
  Unset$p,
  VolatileAttribute,
  delimiter,
  foldlArray,
  foldlArray2,
  nutsToHtml,
  test1
};

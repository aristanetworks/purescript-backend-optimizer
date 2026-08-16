import * as $mAssert from "../Assert/index.js";
import * as $mData$dEq from "../Data.Eq/index.js";
import * as $mData$dShow from "../Data.Show/index.js";
import * as $mSnapshot$dHalogenVDomST01 from "../Snapshot.HalogenVDomST01/index.js";
const assertEqual = /* #__PURE__ */ $mAssert.assertEqual({eq: /* #__PURE__ */ $mData$dEq.eqArrayImpl(ra => rb => ra.a === rb.a && ra.b === rb.b)})({
  show: /* #__PURE__ */ $mData$dShow.showArrayImpl(record => "{ a: " + $mData$dShow.showStringImpl(record.a) + ", b: " + $mData$dShow.showIntImpl(record.b) + " }")
});
const assertEqual3 = /* #__PURE__ */ $mAssert.assertEqual({eq: /* #__PURE__ */ $mData$dEq.eqArrayImpl(ra => rb => ra.a === rb.a && ra.b === rb.b && ra.ix === rb.ix)})({
  show: /* #__PURE__ */ $mData$dShow.showArrayImpl(record => "{ a: " + $mData$dShow.showStringImpl(record.a) + ", b: " + $mData$dShow.showIntImpl(record.b) + ", ix: " + $mData$dShow.showIntImpl(record.ix) + " }")
});
const main = () => {
  const merged1 = [];
  const added1 = [];
  const deleted1 = [];
  const result = $mSnapshot$dHalogenVDomST01.diffWithIxE(
    ["1", "2", "3"],
    [1, 2],
    (ix, a, b) => {
      merged1.push({a, b});
      return {ix, a, b};
    },
    (v, a) => {deleted1.push(a);},
    (ix, b) => {
      added1.push(b);
      return {ix, a: "", b};
    }
  );
  const m1 = [...merged1];
  const a1 = [...added1];
  const d1 = [...deleted1];
  assertEqual("diffWithIxE/merged")({expected: [{a: "1", b: 1}, {a: "2", b: 2}], actual: m1})();
  $mAssert.assertEqual({eq: $mData$dEq.eqArrayImpl($mData$dEq.eqIntImpl)})({show: $mData$dShow.showArrayImpl($mData$dShow.showIntImpl)})("diffWithIxE/added")({
    expected: [],
    actual: a1
  })();
  $mAssert.assertEqual({eq: $mData$dEq.eqArrayImpl($mData$dEq.eqStringImpl)})({show: $mData$dShow.showArrayImpl($mData$dShow.showStringImpl)})("diffWithIxE/deleted")({
    expected: ["3"],
    actual: d1
  })();
  return assertEqual3("diffWithIxE/result")({expected: [{ix: 0, a: "1", b: 1}, {ix: 1, a: "2", b: 2}], actual: result})();
};
export {assertEqual, assertEqual3, main};

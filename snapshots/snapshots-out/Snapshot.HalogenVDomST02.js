import * as Assert from "../Assert/index.js";
import * as Data$dArray$dST from "../Data.Array.ST/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Snapshot$dHalogenVDomST01 from "../Snapshot.HalogenVDomST01/index.js";
const assertEqual = /* #__PURE__ */ Assert.assertEqual({eq: /* #__PURE__ */ Data$dEq.eqArrayImpl(ra => rb => ra.a === rb.a && ra.b === rb.b)})({
  show: /* #__PURE__ */ Data$dShow.showArrayImpl(record => "{ a: " + Data$dShow.showStringImpl(record.a) + ", b: " + Data$dShow.showIntImpl(record.b) + " " + "}")
});
const assertEqual3 = /* #__PURE__ */ Assert.assertEqual({eq: /* #__PURE__ */ Data$dEq.eqArrayImpl(ra => rb => ra.a === rb.a && ra.b === rb.b && ra.ix === rb.ix)})({
  show: /* #__PURE__ */ Data$dShow.showArrayImpl(record => "{ a: " + Data$dShow.showStringImpl(record.a) + ", b: " + Data$dShow.showIntImpl(record.b) + ", ix: " + Data$dShow.showIntImpl(record.ix) + " " + "}")
});
const main = () => {
  const merged1 = [];
  const added1 = [];
  const deleted1 = [];
  const result = Snapshot$dHalogenVDomST01.diffWithIxE(
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
  const m1 = Data$dArray$dST.freeze(merged1)();
  const a1 = Data$dArray$dST.freeze(added1)();
  const d1 = Data$dArray$dST.freeze(deleted1)();
  assertEqual("diffWithIxE/merged")({expected: [{a: "1", b: 1}, {a: "2", b: 2}], actual: m1})();
  Assert.assertEqual({eq: Data$dEq.eqArrayImpl(Data$dEq.eqIntImpl)})({show: Data$dShow.showArrayImpl(Data$dShow.showIntImpl)})("diffWithIxE/added")({expected: [], actual: a1})();
  Assert.assertEqual({eq: Data$dEq.eqArrayImpl(Data$dEq.eqStringImpl)})({show: Data$dShow.showArrayImpl(Data$dShow.showStringImpl)})("diffWithIxE/deleted")({
    expected: ["3"],
    actual: d1
  })();
  return assertEqual3("diffWithIxE/result")({expected: [{ix: 0, a: "1", b: 1}, {ix: 1, a: "2", b: 2}], actual: result})();
};
export {assertEqual, assertEqual3, main};

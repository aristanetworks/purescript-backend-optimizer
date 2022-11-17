import * as $runtime from "../runtime.js";
import * as Data$dEuclideanRing from "../Data.EuclideanRing/index.js";
const test8 = /* #__PURE__ */ (() => {
  const arr = [];
  for (const a of [1, 2, 3]) {
    if (a < 10) {
      arr.push(a);
      continue;
    }
    arr.push(12);
  }
  return arr;
})();
const test7 = bool => (() => {
  const arr = [];
  const $$break = {value: true};
  if (bool) {
    while ($$break.value) {
      arr.push(1);
      $$break.value = false;
    }
    return;
  }
})();
const test6 = /* #__PURE__ */ (() => {
  const arr = [];
  const $$break = {value: true};
  while ($$break.value) {
    arr.push(1);
    $$break.value = false;
  }
})();
const test5 = /* #__PURE__ */ (() => {
  const arr = [];
  const $$break = {value: true};
  while ($$break.value) {
    arr.push(1);
    $$break.value = false;
  }
  return arr;
})();
const test4 = /* #__PURE__ */ (() => {
  const arr = [];
  for (const a of $runtime.range(1, 1000)) {
    if (a === (Data$dEuclideanRing.intDiv(a)(2) * 2 | 0)) { arr.push(a); }
  }
  return arr;
})();
const test3 = /* #__PURE__ */ (() => {
  const arr = [];
  for (const a of [1, 2, 3]) {
    if (a < 10) { arr.push(a); }
  }
  return arr;
})();
const test2 = /* #__PURE__ */ (() => {
  const arr = [];
  const n = arr.push(1);
  arr.push(2, n);
  return arr;
})();
const test1 = /* #__PURE__ */ (() => {
  const $0 = [];
  return $0;
})();
export {test1, test2, test3, test4, test5, test6, test7, test8};

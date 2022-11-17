const test3 = /* #__PURE__ */ (() => {
  const nRef = {value: 1};
  const mRef = {value: 2};
  const n = nRef.value;
  const m = mRef.value;
  return n + m | 0;
})();
const test2 = 3;
const test1 = 1;
export {test1, test2, test3};

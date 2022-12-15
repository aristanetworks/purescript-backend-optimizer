const test3 = /* #__PURE__ */ (() => {
  let nRef = 1;
  let mRef = 2;
  const n = nRef;
  const m = mRef;
  return n + m | 0;
})();
const test2 = 3;
const test1 = 1;
export {test1, test2, test3};

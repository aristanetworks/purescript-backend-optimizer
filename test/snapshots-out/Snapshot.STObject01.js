const test6 = /* #__PURE__ */ (() => {
  const $0 = {};
  $0.a = 1;
  $0.b = 2;
  delete $0.a;
  delete $0.b;
  return $0;
})();
const test5 = /* #__PURE__ */ (() => {
  const obj = {};
  obj.a = 1;
  obj.b = 2;
  delete obj.a;
  delete obj.b;
  return obj;
})();
const test4 = /* #__PURE__ */ (() => {
  const $0 = {};
  $0.a = 1;
  $0.b = 2;
  $0.c = 3;
  return $0;
})();
const test3 = /* #__PURE__ */ (() => {
  const obj = {};
  obj.a = 1;
  obj.b = 2;
  obj.c = 3;
  return obj;
})();
const test2 = /* #__PURE__ */ (() => {
  const obj = {};
  return {...obj};
})();
const test1 = {};
export {test1, test2, test3, test4, test5, test6};

// @inline export f never
const test1 = a => {
  if (a) { return [1, 2, 3]; }
  return [];
};
const f = x => x;
const test2 = /* #__PURE__ */ (() => {
  const $0 = f([1, 2, 3]);
  return a => {
    if (a) { return $0; }
    return [];
  };
})();
export {f, test1, test2};

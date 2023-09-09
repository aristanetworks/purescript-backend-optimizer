const test = x => {
  const $0 = x ? (a => () => a.push(1)) : a => () => a.unshift(2);
  const arr = [];
  $0(arr)();
  return arr;
};
export {test};

const test2 = f => {
  const $0 = f([1, 2, 3]);
  return a => {
    if (a) { return $0; }
    return [];
  };
};
const test1 = a => {
  if (a) { return [1, 2, 3]; }
  return [];
};
export {test1, test2};

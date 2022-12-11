const test2 = f => as => {
  const bs = [];
  for (const a of as) {
    for (const $0 of f(a)) {
      bs.push($0);
    }
  }
  return bs;
};
const test1 = f => as => {
  const bs = [];
  for (const a of as) {
    bs.push(f(a));
  }
  return bs;
};
export {test1, test2};

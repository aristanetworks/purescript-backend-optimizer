const test1 = f => as => () => {
  const bs = [];
  for (const a of as) {
    bs.push(f(a));
  }
};
export {test1};

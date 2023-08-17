const test = random => () => {
  const x = random();
  const x1 = random();
  const y = random();
  const m = random();
  return ((x + x1 | 0) + y | 0) - m | 0;
};
export {test};

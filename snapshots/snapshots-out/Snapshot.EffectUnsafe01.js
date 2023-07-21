const test2 = random => {
  const n = random();
  const m = random();
  return n + m | 0;
};
const test1 = 1;
export {test1, test2};

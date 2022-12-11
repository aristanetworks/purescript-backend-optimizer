import * as Data$dTuple from "../Data.Tuple/index.js";
const test3 = () => {
  const count = {value: 0};
  return Data$dTuple.$Tuple(
    count,
    n => () => {
      const $0 = count.value;
      count.value = $0 + n | 0;
    }
  );
};
const test2 = () => {
  let count = 0;
  return n => () => {
    const $0 = count;
    count = $0 + n | 0;
  };
};
const test1 = hi => () => {
  let count = 0;
  let $$continue = true;
  while ($$continue) {
    const n = count;
    if (n < hi) {
      count = n + 1 | 0;
      continue;
    }
    $$continue = false;
  }
  return count;
};
export {test1, test2, test3};

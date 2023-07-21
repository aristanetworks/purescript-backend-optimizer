const test2 = a => {
  const $0 = a + 1 | 0;
  return () => $0;
};
const test1 = () => 1;
export {test1, test2};

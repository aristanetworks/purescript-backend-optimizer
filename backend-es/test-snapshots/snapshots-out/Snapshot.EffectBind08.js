const test = () => {
  const a = (v => 12)();
  return a + 1 | 0;
};
export {test};

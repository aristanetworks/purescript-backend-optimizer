const test1 = a => b => c => {
  if (a && b) { return 1; }
  if (c) { return 2; }
  return 3;
};
export {test1};

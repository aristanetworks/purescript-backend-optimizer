const test1 = v => {
  if (v.length === 0) { return "0"; }
  if (v.length === 1) { return "1"; }
  if (v.length === 2) {
    if (v[1] === 2) { return "2"; }
    return "catch";
  }
  if (v.length === 3) { return "3"; }
  return "catch";
};
export {test1};

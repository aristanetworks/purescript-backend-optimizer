const test6 = v => {
  if (v.a > 0) { return v.a; }
  if (v.b > 0) { return v.b; }
  return 0;
};
const test5 = v => {
  if (v.a > 0) { return v.a; }
  if (v.b > 0) { return v.b; }
  return 0;
};
const test4 = v => {
  if (v.a > 0) { return v.a; }
  if (v.b > 1) { return v.b; }
  return 3;
};
const test3 = v => {
  if (v.a > 0) { return v.a; }
  if (v.b > 1) { return v.b; }
  return 3;
};
const test2 = v => {
  if (v.a.c === 2) {
    if (v.d.e === 1) {
      if (v.d.f === 2) {
        if (v.a.b === 1) { return 1; }
        return 2;
      }
      if (v.a.b === 1) { return 3; }
      return 4;
    }
    if (v.a.b === 1) { return 3; }
    return 4;
  }
  return 4;
};
const test1 = v => {
  if (v.a === 1) { return "0"; }
  if (v.b === 1) { return "1"; }
  if (v.c === 1) { return "2"; }
  if (v.a === 2) {
    if (v.b === 2) { return "3"; }
    return "catch";
  }
  return "catch";
};
export {test1, test2, test3, test4, test5, test6};

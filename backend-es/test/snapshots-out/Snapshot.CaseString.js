const test1 = v => {
  if (v === "foo") { return "1"; }
  if (v === "bar") { return "2"; }
  if (v === "") { return "3"; }
  return "catch";
};
export {test1};

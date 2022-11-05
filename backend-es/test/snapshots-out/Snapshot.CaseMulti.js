const test1 = v => v1 => {
  if (v === 1) {
    if (v1 === 1) { return "1.1"; }
    if (v1 === 2) { return "1.2"; }
    if (v1 === 3) { return "1.3"; }
    if (v1 === 4) { return "_.4"; }
    if (v1 === 5) { return "1.5"; }
    return "_._";
  }
  if (v1 === 4) { return "_.4"; }
  if (v1 === 2) { return "_.2"; }
  return "_._";
};
export {test1};

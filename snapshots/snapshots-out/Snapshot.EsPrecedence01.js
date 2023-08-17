const test1 = f => a => b => {
  if (f(a) && f(b) ? f(a) : true) { return f(a); }
  return f();
};
export {test1};

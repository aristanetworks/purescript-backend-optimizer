export const time_ = name => k => {
  console.time(name);
  const res = k();
  console.timeEnd(name);
  return res;
};

export const traceImpl = a => k => {
  console.dir(a, { depth: null });
  return k();
};

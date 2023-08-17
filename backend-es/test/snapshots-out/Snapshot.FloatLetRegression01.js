const test = f => {
  const c = f(2);
  return {b: f(1), c1: c, c2: c};
};
export {test};

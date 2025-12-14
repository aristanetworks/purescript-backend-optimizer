const test = f => y => {
  const z = f(y);
  return {bar: z - 2 | 0, foo: z + 1 | 0};
};
export {test};

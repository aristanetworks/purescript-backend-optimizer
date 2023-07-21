const test9 = g => {
  const $0 = g(42);
  return () => {
    let ref = $0;
    const prev = ref;
    ref = prev + 1 | 0;
    const $1 = ref;
    ref = 1 + $1 | 0;
    return ref;
  };
};
const test8 = g => r => {
  const $0 = g(g);
  return () => {
    const $1 = r.value;
    return r.value = $0($1);
  };
};
const test7 = g => r => () => {
  const $0 = r.value;
  return r.value = g($0);
};
const test6 = g => r => {
  const $0 = g(42);
  return () => r.value = $0;
};
const test5 = r => () => r.value = 42;
const test4 = g => r => {
  const $0 = g(r);
  return () => $0.value;
};
const test3 = r => () => r.value;
const test2 = g => {
  const $0 = g(42);
  return () => ({value: $0});
};
const test1 = () => ({value: 42});
export {test1, test2, test3, test4, test5, test6, test7, test8, test9};

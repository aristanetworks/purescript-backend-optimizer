const test4 = ref => arr => () => {
  for (const a of arr) {
    if (a < 10) {
      const $0 = ref.value;
      ref.value = $0 + a | 0;
      continue;
    }
    const $0 = ref.value;
    ref.value = $0 + 1 | 0;
  }
};
const test3 = ref => arr => () => {
  for (const a of arr) {
    if (a < 10) {
      const $0 = ref.value;
      ref.value = $0 + a | 0;
    }
  }
};
const test2 = ref => k => {
  const $0 = k(42);
  return () => {
    for (const a of $0) {
      const $1 = ref.value;
      ref.value = $1 + a | 0;
    }
    for (const $1 of k(42)) {
      const $2 = ref.value;
      ref.value = $1 + $2 | 0;
    }
    for (const $1 of k(42)) {
      const $2 = ref.value;
      ref.value = $2 + 1 | 0;
    }
  };
};
const test1 = ref => k => {
  const $0 = k(42);
  return () => {
    for (const a of $0) {
      const $1 = ref.value;
      ref.value = $1 + a | 0;
      const $2 = ref.value;
      ref.value = $2 + a | 0;
    }
  };
};
export {test1, test2, test3, test4};

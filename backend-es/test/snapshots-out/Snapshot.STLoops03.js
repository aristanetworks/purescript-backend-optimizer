const test4 = cond => ref => () => {
  while (cond.value) {
    const a = ref.value;
    if (a < 10) {
      const $0 = ref.value;
      ref.value = $0 + 1 | 0;
      continue;
    }
    const $0 = ref.value;
    ref.value = $0 + 2 | 0;
  }
};
const test3 = cond => ref => () => {
  while (cond.value) {
    const a = ref.value;
    if (a < 10) {
      const $0 = ref.value;
      ref.value = $0 + 1 | 0;
    }
  }
};
const test2 = cond => ref => () => {
  while (cond.value) {
    const $0 = ref.value;
    ref.value = $0 + 1 | 0;
  }
  while (cond.value) {
    const $0 = ref.value;
    ref.value = $0 + 2 | 0;
  }
};
const test1 = cond => ref => () => {
  while (cond.value) {
    const $0 = ref.value;
    ref.value = $0 + 1 | 0;
    const $1 = ref.value;
    ref.value = $1 + 2 | 0;
  }
};
export {test1, test2, test3, test4};

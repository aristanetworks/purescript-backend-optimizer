import * as $runtime from "../runtime.js";
const test4 = ref => lo => hi => () => {
  for (const a of $runtime.range(lo, hi)) {
    if (a < 10) {
      const $0 = ref.value;
      ref.value = $0 + a | 0;
      continue;
    }
    const $0 = ref.value;
    ref.value = $0 + 1 | 0;
  }
};
const test3 = ref => lo => hi => () => {
  for (const a of $runtime.range(lo, hi)) {
    if (a < 10) {
      const $0 = ref.value;
      ref.value = $0 + a | 0;
    }
  }
};
const test2 = ref => lo => hi => {
  const $0 = lo + 1 | 0;
  const $1 = hi + 1 | 0;
  return () => {
    for (const a of $runtime.range($0, $1)) {
      const $2 = ref.value;
      ref.value = $2 + a | 0;
    }
    for (const $2 of $runtime.range(lo + 1 | 0, hi + 1 | 0)) {
      const $3 = ref.value;
      ref.value = $2 + $3 | 0;
    }
    for (const $2 of $runtime.range(lo + 1 | 0, hi + 1 | 0)) {
      const $3 = ref.value;
      ref.value = $3 + 1 | 0;
    }
  };
};
const test1 = ref => lo => hi => {
  const $0 = lo + 1 | 0;
  const $1 = hi + 1 | 0;
  return () => {
    for (const a of $runtime.range($0, $1)) {
      const $2 = ref.value;
      ref.value = $2 + a | 0;
      const $3 = ref.value;
      ref.value = $3 + a | 0;
    }
  };
};
export {test1, test2, test3, test4};

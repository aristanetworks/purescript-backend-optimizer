const test = f => {
  const ref = {value: 0};
  const $0 = f(ref);
  return () => {
    const $1 = $0.value;
    $0.value = 1 + $1 | 0;
    const $2 = ref.value;
    ref.value = 1 + $2 | 0;
  };
};
export {test};

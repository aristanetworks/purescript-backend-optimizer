const test = f => {
  const ref = {value: 0};
  const $2 = f(ref);
  return () => {
    const $3 = $2.value;
    $2.value = 1 + $3 | 0;
    const $5 = ref.value;
    ref.value = 1 + $5 | 0;
  };
};
export {test};

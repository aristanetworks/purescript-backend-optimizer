export function binding(init) {
  let state = 0;
  let value;

  return () => {
    if (state === 2) {
      return value;
    }
    if (state === 1) {
      throw new Error("Binding demanded before initialized");
    }
    state = 1;
    value = init();
    state = 2;
    return value;
  };
}

export function* range(lo, hi) {
  for (let i = lo; i < hi; i++) {
    yield i;
  }
}

export function fail() {
  throw new Error("Failed pattern match");
}

export function intDiv(x, y) {
  if (y > 0) return Math.floor(x / y);
  if (y < 0) return -Math.floor(x / -y);
  return 0;
}

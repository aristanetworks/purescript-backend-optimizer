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

export function fail() {
  throw new Error("Failed pattern match");
}

export function recordUnionMutateLeft(lhs, rhs) {
  for (const [key, value] of Object.entries(rhs)) {
    if (Reflect.has(lhs, key)) {
      continue;
    }
    lhs[key] = value;
  }
  return lhs;
}

export function recordUnionMutateRight(lhs, rhs) {
  for (const [key, value] of Object.entries(lhs)) {
    rhs[key] = value;
  }
  return rhs;
}

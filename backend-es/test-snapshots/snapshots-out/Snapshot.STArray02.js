const test2 = inp => {
  const arr = [];
  const n = arr.unshift(1);
  arr.unshift(1, n);
  arr.unshift(...inp);
  arr.unshift(1, 2, 3, ...inp);
  arr.unshift(...inp, 2, 3, 4);
  arr.unshift(1, 2, 3, ...inp, 5, 6, 7);
  return arr;
};
const test1 = inp => {
  const arr = [];
  const n = arr.push(1);
  arr.push(1, n);
  arr.push(...inp);
  arr.push(1, 2, 3, ...inp);
  arr.push(...inp, 2, 3, 4);
  arr.push(1, 2, 3, ...inp, 5, 6, 7);
  return arr;
};
export {test1, test2};

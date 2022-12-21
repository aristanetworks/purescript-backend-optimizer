const $Box = _1 => ({tag: "Box", _1});
const Box = value0 => $Box(value0);
const test3 = v => {
  const $0 = v._1;
  return $Box(b => $0 + b | 0);
};
const test2 = v => {
  const $0 = v._1;
  return b => $0 + b | 0;
};
const test1 = v => b => v._1 + b | 0;
export {$Box, Box, test1, test2, test3};

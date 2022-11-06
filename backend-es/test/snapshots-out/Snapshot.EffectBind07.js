import * as Data$dUnit from "../Data.Unit/index.js";
const test = random => value => () => {
  const x = random();
  const c = value(Data$dUnit.unit);
  const b = c + c | 0;
  const a = b + b | 0;
  const x1 = random();
  const y = random();
  const m = random();
  return (x + (((x1 + y | 0) + a | 0) + a | 0) | 0) - m | 0;
};
export {test};

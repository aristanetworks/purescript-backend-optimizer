import * as Data$dShow from "../Data.Show/index.js";
const $Product3 = (_1, _2, _3) => ({tag: "Product3", _1, _2, _3});
const Product3 = value0 => value1 => value2 => $Product3(value0, value1, value2);
const test2 = v => {
  if (v._1 === 1) { return Data$dShow.showIntImpl(v._2) + Data$dShow.showIntImpl(v._3) + Data$dShow.showIntImpl(v._1); }
  if (v._2 === 1) { return Data$dShow.showIntImpl(v._1) + Data$dShow.showIntImpl(v._3) + Data$dShow.showIntImpl(v._2); }
  if (v._3 === 1) { return Data$dShow.showIntImpl(v._1) + Data$dShow.showIntImpl(v._2) + Data$dShow.showIntImpl(v._3); }
  return Data$dShow.showIntImpl(v._1) + Data$dShow.showIntImpl(v._1) + Data$dShow.showIntImpl(v._2) + Data$dShow.showIntImpl(v._2) + Data$dShow.showIntImpl(v._3) + Data$dShow.showIntImpl(v._3);
};
const test1 = v => {
  if (v === 1) { return Data$dShow.showIntImpl(v) + Data$dShow.showIntImpl(v) + Data$dShow.showIntImpl(v); }
  if (v === 2) { return Data$dShow.showIntImpl(v); }
  return "any: " + Data$dShow.showIntImpl(v) + Data$dShow.showIntImpl(v) + Data$dShow.showIntImpl(v);
};
export {$Product3, Product3, test1, test2};

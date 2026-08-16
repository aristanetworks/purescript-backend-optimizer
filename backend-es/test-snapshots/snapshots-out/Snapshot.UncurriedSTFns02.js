import * as $mData$dShow from "../Data.Show/index.js";
import * as $mEffect$dConsole from "../Effect.Console/index.js";
const test2 = random => () => {
  const n = random();
  return $mEffect$dConsole.log($mData$dShow.showIntImpl(n))();
};
const test1 = () => 12;
export {test1, test2};

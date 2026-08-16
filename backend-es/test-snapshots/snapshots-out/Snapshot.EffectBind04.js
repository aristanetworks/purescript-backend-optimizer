import * as $mEffect$dConsole from "../Effect.Console/index.js";
const test2 = random => () => {
  const n = random();
  if (n > 100) {
    $mEffect$dConsole.log("Too hot")();
  } else if (n < 20) {
    $mEffect$dConsole.log("Too cold")();
  } else {
    $mEffect$dConsole.log("Just right")();
  }
  return $mEffect$dConsole.log("Done")();
};
const test1 = random => () => {
  const n = random();
  if (n > 100) { return $mEffect$dConsole.log("Too hot")(); }
  if (n < 20) { return $mEffect$dConsole.log("Too cold")(); }
  return $mEffect$dConsole.log("Just right")();
};
export {test1, test2};

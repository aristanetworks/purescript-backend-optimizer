import * as Effect$dConsole from "../Effect.Console/index.js";
const test2 = random => () => {
  const n = random();
  if (n > 100) {
    Effect$dConsole.log("Too hot")();
  } else if (n < 20) {
    Effect$dConsole.log("Too cold")();
  } else {
    Effect$dConsole.log("Just right")();
  }
  return Effect$dConsole.log("Done")();
};
const test1 = random => () => {
  const n = random();
  if (n > 100) { return Effect$dConsole.log("Too hot")(); }
  if (n < 20) { return Effect$dConsole.log("Too cold")(); }
  return Effect$dConsole.log("Just right")();
};
export {test1, test2};

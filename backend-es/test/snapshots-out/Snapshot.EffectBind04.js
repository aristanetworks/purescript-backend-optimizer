import * as Effect$dConsole from "../Effect.Console/index.js";
import {random} from "./foreign.js";
const test = () => {
  const n = random();
  if (n > 100) { return Effect$dConsole.log("Too hot")(); }
  if (n < 20) { return Effect$dConsole.log("Too cold")(); }
  return Effect$dConsole.log("Just right")();
};
export {test};
export * from "./foreign.js";

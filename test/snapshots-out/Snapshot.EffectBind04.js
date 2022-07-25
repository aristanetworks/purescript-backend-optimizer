import * as $runtime from "../runtime.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dClass$dConsole from "../Effect.Class.Console/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Snapshot$dEffectBind04$foreign from "./foreign.js";
const random = Snapshot$dEffectBind04$foreign.random;
const test = () => {
  const n = random();
  if (n > 100) { return Effect$dConsole.log("Too hot")(); }
  if (n < 20) { return Effect$dConsole.log("Too cold")(); }
  return Effect$dConsole.log("Just right")();
};
export {random, test};
export * from "./foreign.js";

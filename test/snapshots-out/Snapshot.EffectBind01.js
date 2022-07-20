import * as $runtime from "../runtime.js";
import * as Effect from "../Effect/index.js";
import * as Effect$dClass from "../Effect.Class/index.js";
import * as Effect$dClass$dConsole from "../Effect.Class.Console/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test1 = () => {
  Effect$dConsole.log("1")();
  const value = Effect$dConsole.log("2")();
  Effect$dConsole.log("3")();
  return value;
};
export {test1};

import * as $runtime from "./runtime.js";
import * as Effect from "./Effect.js";
import * as Effect$dClass from "./Effect.Class.js";
import * as Effect$dClass$dConsole from "./Effect.Class.Console.js";
import * as Effect$dConsole from "./Effect.Console.js";
const test1 = () => {
  Effect$dConsole.log("1")();
  const value = Effect$dConsole.log("2")();
  Effect$dConsole.log("3")();
  return value;
};
export {test1};

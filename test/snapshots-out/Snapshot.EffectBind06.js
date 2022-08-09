import * as $runtime from "../runtime.js";
import {random} from "./foreign.js";
const test = () => {
  const x = random();
  const x1 = random();
  const y = random();
  const m = random();
  return (x + (x1 + y | 0) | 0) - m | 0;
};
export {test};
export * from "./foreign.js";

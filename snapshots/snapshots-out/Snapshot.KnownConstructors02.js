import * as $runtime from "../runtime.js";
const test = a => {
  if (a.tag === "Left") { return a._1; }
  if (a.tag === "Right") { return a._1; }
  $runtime.fail();
};
export {test};

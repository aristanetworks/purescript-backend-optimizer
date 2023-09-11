import * as $runtime from "../runtime.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const test = eff => () => {
  const res = eff();
  if (res.tag === "Nothing") { return; }
  if (res.tag === "Just") {
    for (const a of res._1) {
      Effect$dConsole.log(a)();
    }
    return;
  }
  $runtime.fail();
};
export {test};

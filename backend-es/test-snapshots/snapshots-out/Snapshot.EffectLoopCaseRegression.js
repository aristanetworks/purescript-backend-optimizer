import * as $runtime from "../runtime.js";
import * as $mEffect$dConsole from "../Effect.Console/index.js";
const test = eff => () => {
  const res = eff();
  if (res.tag === "Nothing") { return; }
  if (res.tag === "Just") {
    for (const a of res._1) {
      $mEffect$dConsole.log(a)();
    }
    return;
  }
  $runtime.fail();
};
export {test};

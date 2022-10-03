import * as $runtime from "../runtime.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
const main = dictMonadEffect => dictMonadEffect.liftEffect(Effect$dConsole.log("1"));
export {main};

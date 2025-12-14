import * as Effect$dConsole from "../Effect.Console/index.js";
const when$p = bool => k => {
  if (bool) { return k(); }
  return () => {};
};
const test1 = bool => {
  const $0 = bool ? Effect$dConsole.log("1") : () => {};
  return () => {
    $0();
    if (bool) { Effect$dConsole.log("2")(); }
    if (bool) { return Effect$dConsole.log("3")(); }
  };
};
export {test1, when$p};

import * as Effect$dConsole from "../Effect.Console/index.js";
const test1 = /* #__PURE__ */ (() => {
  const b = Effect$dConsole.log("foo");
  return {l: 5, r: {l: {l: 1, r: {l: b, r: {l: 5, r: b}}}, r: 6}};
})();
export {test1};

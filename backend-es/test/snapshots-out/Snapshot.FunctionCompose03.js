import * as Data$dUnit from "../Data.Unit/index.js";
import {f, g} from "./foreign.js";
const test4 = /* #__PURE__ */ (() => {
  const $0 = g(Data$dUnit.unit);
  const $1 = f(Data$dUnit.unit);
  const $2 = g(Data$dUnit.unit);
  const $3 = f(Data$dUnit.unit);
  const $4 = g(Data$dUnit.unit);
  return x => $0($1($2($3($4(x)))));
})();
const test3 = /* #__PURE__ */ (() => {
  const $0 = f(Data$dUnit.unit);
  const $1 = g(Data$dUnit.unit);
  const $2 = f(Data$dUnit.unit);
  const $3 = g(Data$dUnit.unit);
  return x => $0($1($2($3(x))));
})();
const test2 = /* #__PURE__ */ (() => {
  const $0 = g(Data$dUnit.unit);
  const $1 = f(Data$dUnit.unit);
  const $2 = g(Data$dUnit.unit);
  return x => $0($1($2(x)));
})();
const test1 = /* #__PURE__ */ (() => {
  const $0 = f(Data$dUnit.unit);
  const $1 = g(Data$dUnit.unit);
  return x => $0($1(x));
})();
export {test1, test2, test3, test4};
export * from "./foreign.js";

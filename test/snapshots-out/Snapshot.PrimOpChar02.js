import * as $runtime from "../runtime.js";
const charValues = op => [op('a')('a'), op('a')('b'), op('b')('a')];
const test1 = [true, false, false];
const test2 = [false, true, true];
const test3 = [false, true, false];
const test4 = [false, false, true];
const test5 = [true, true, false];
const test6 = [true, false, true];
export {charValues, test1, test2, test3, test4, test5, test6};

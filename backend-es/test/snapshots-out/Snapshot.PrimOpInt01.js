import * as $runtime from "../runtime.js";
const test9 = a => b => a * b | 0;
const test8 = a => b => a >= b;
const test7 = a => b => a <= b;
const test6 = a => b => a > b;
const test5 = a => b => a < b;
const test4 = a => b => a !== b;
const test3 = a => b => a === b;
const test2 = a => b => a - b | 0;
const test11 = a => -a;
const test10 = a => b => $runtime.intDiv(a, b);
const test1 = a => b => a + b | 0;
export {test1, test10, test11, test2, test3, test4, test5, test6, test7, test8, test9};

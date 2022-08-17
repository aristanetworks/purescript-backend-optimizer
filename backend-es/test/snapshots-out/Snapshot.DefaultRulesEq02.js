import * as $runtime from "../runtime.js";
const test6 = y => !("hello" === y.bar && (!y.baz && 42 === y.foo));
const test5 = y => 12 !== y;
const test4 = a => a !== 12;
const test3 = a => 12 !== a;
const test2 = a => b => a !== b;
const test1 = x => y => x !== y;
export {test1, test2, test3, test4, test5, test6};

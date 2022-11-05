import {f, g} from "./foreign.js";
const test6 = a => a;
const test5 = a => v => a;
const test4 = b => a => f(a)(b);
const test3 = a => f(g(1)(2))(3);
const test2 = a => f(1)(g("foo")(a));
const test1 = a => f(1)(g("foo")(a));
export {test1, test2, test3, test4, test5, test6};
export * from "./foreign.js";

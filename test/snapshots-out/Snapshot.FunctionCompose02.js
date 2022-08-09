import * as $runtime from "../runtime.js";
import {f, g} from "./foreign.js";
const test4 = x => g(f(g(f(g(x)))));
const test3 = x => f(g(f(g(x))));
const test2 = x => g(f(g(x)));
const test1 = x => f(g(x));
export {test1, test2, test3, test4};
export * from "./foreign.js";

import * as $runtime from "../runtime.js";
import {f, g} from "./foreign.js";
const test2 = x => f(x) + (g(x) + (f(x) + g(x)));
const test1 = x => f(x) + g(x);
export {test1, test2};
export * from "./foreign.js";

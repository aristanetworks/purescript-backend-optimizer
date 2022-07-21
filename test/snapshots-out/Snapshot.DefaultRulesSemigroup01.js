import * as $runtime from "../runtime.js";
import * as Snapshot$dDefaultRulesSemigroup01$foreign from "./foreign.js";
const f = Snapshot$dDefaultRulesSemigroup01$foreign.f;
const g = Snapshot$dDefaultRulesSemigroup01$foreign.g;
const test2 = x => f(x) + (g(x) + (f(x) + g(x)));
const test1 = x => f(x) + g(x);
export {f, g, test1, test2};
export * from "./foreign.js";

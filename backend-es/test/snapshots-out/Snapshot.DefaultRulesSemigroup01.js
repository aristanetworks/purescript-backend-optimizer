// @inline export f never
// @inline export g never
const g = v => "???";
const f = v => "???";
const test1 = x => f(x) + g(x);
const test2 = x => f(x) + g(x) + f(x) + g(x);
export {f, g, test1, test2};

// @inline export f never
// @inline export g never
const identity = x => x;
const g = v => identity;
const f = v => identity;
const test1 = x => f()(g()(x));
const test2 = x => g()(f()(g()(x)));
const test3 = x => f()(g()(f()(g()(x))));
const test4 = x => g()(f()(g()(f()(g()(x)))));
export {f, g, identity, test1, test2, test3, test4};

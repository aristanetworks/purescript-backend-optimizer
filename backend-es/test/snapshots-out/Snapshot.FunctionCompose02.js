const test4 = f => g => x => g(f(g(f(g(x)))));
const test3 = f => g => x => f(g(f(g(x))));
const test2 = f => g => x => g(f(g(x)));
const test1 = f => g => x => f(g(x));
export {test1, test2, test3, test4};

const test2 = f => g => x => f(x) + g(x) + f(x) + g(x);
const test1 = f => g => x => f(x) + g(x);
export {test1, test2};

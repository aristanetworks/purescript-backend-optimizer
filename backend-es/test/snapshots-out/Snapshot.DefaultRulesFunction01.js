const test6 = a => a;
const test5 = a => v => a;
const test4 = f => b => a => f(a)(b);
const test3 = f => g => v => f(g(1)(2))(3);
const test2 = f => g => a => f(1)(g("foo")(a));
const test1 = f => g => a => f(1)(g("foo")(a));
export {test1, test2, test3, test4, test5, test6};

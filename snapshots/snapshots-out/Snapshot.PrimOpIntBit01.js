const test7 = a => ~a;
const test6 = a => b => a >>> b;
const test5 = a => b => a ^ b;
const test4 = a => b => a >> b;
const test3 = a => b => a << b;
const test2 = a => b => a | b;
const test1 = a => b => a & b;
export {test1, test2, test3, test4, test5, test6, test7};

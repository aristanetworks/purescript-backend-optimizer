const test2 = a => b => a >> (b >> b);
const test1 = a => b => a >> b >> b;
export {test1, test2};

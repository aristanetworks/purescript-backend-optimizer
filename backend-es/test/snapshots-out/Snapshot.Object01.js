const test5 = obj => Object.hasOwn(obj, "wat");
const test4 = obj => Object.keys(obj);
const test3 = a => b => a[b];
const test2 = a => a["foo.bar"];
const test1 = a => a.foo;
export {test1, test2, test3, test4, test5};

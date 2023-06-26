// @inline Snapshot.RecursionInlined03.compose always
const id = a => a;
const compose = ab => bc => a => bc(ab(a));
const test1 = a => a;
const test2 = z => a => z(a);
export {compose, id, test1, test2};

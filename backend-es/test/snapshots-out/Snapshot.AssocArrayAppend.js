const test3 = x => ["a", "b", ...x, ...x, ...x, ...x, "c", "d", "e", ...x, ...x, ...x, ...x, "f", "g"];
const test2 = x => ["a", "b", ...x, ...x, ...x, ...x, "c", "d"];
const test1 = x => ["a", "b", ...x, ...x, ...x, ...x, "c", "d"];
export {test1, test2, test3};

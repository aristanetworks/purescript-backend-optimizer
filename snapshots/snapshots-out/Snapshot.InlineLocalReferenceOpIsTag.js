const $List = (tag, _1, _2) => ({tag, _1, _2});
const Cons = value0 => value1 => $List("Cons", value0, value1);
const Nil = /* #__PURE__ */ $List("Nil");
const test = fn => $List("Cons", 0, $List("Cons", 1, fn({})));
export {$List, Cons, Nil, test};

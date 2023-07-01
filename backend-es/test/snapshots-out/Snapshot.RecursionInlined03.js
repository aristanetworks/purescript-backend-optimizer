// @inline Snapshot.RecursionInlined03.append always
// This doesn't quite work yet, because the inlining of append depends
// on the analysis of the local `b`, which we don't have (easy) access to.
// If we can somehow grab its usage, we'd likely see that access + case == total
import * as Partial from "../Partial/index.js";
const List = x => x;
const nil = {type: "nil", value: undefined};
const cons = head => tail => ({type: "cons", value: {head, tail}});
const append = v => b => {
  if (v.type === "cons") { return {type: "cons", value: {head: v.value.head, tail: append(v.value.tail)(b)}}; }
  if (v.type === "nil") { return b; }
  return Partial._crashWith("Data.Variant: pattern match failure [" + v.type + "]");
};
const test1 = {
  type: "cons",
  value: {
    head: "a",
    tail: {
      type: "cons",
      value: {
        head: "b",
        tail: {
          type: "cons",
          value: {
            head: "c",
            tail: {
              type: "cons",
              value: {head: "d", tail: {type: "cons", value: {head: "e", tail: {type: "cons", value: {head: "f", tail: {type: "cons", value: {head: "g", tail: nil}}}}}}}
            }
          }
        }
      }
    }
  }
};
const test2 = z => (
  {
    type: "cons",
    value: {
      head: "a",
      tail: {
        type: "cons",
        value: {
          head: "b",
          tail: {
            type: "cons",
            value: {
              head: "c",
              tail: append(z)({
                type: "cons",
                value: {head: "d", tail: {type: "cons", value: {head: "e", tail: {type: "cons", value: {head: "f", tail: {type: "cons", value: {head: "g", tail: nil}}}}}}}
              })
            }
          }
        }
      }
    }
  }
);
export {List, append, cons, nil, test1, test2};

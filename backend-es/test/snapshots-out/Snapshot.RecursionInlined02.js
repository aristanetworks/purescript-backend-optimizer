// @inline Snapshot.RecursionInlined02.append always
import * as $runtime from "../runtime.js";
const $List = (tag, _1, _2) => ({tag, _1, _2});
const Nil = /* #__PURE__ */ $List("Nil");
const Cons = value0 => value1 => $List("Cons", value0, value1);
const append = v => v1 => {
  if (v.tag === "Nil") { return v1; }
  if (v.tag === "Cons") { return $List("Cons", v._1, append(v._2)(v1)); }
  $runtime.fail();
};
const test1 = z => $List(
  "Cons",
  "a",
  $List(
    "Cons",
    "b",
    $List(
      "Cons",
      "c",
      $List(
        "Cons",
        "d",
        $List(
          "Cons",
          "e",
          $List(
            "Cons",
            "f",
            $List(
              "Cons",
              "g",
              (() => {
                if (z.tag === "Nil") { return Nil; }
                if (z.tag === "Cons") { return $List("Cons", z._1, append(z._2)(Nil)); }
                $runtime.fail();
              })()
            )
          )
        )
      )
    )
  )
);
export {$List, Cons, Nil, append, test1};

import * as $runtime from "../runtime.js";
const $Html = (tag, _1, _2) => ({tag, _1, _2});
const Elem = value0 => value1 => $Html("Elem", value0, value1);
const Text = value0 => $Html("Text", value0);
const h11 = /* #__PURE__ */ (() => {
  const _0 = Elem("h1");
  return x => _0([x]);
})();
const h21 = /* #__PURE__ */ (() => {
  const _0 = Elem("h2");
  return x => _0([x]);
})();
const p1 = /* #__PURE__ */ Elem("p");
const section1 = /* #__PURE__ */ Elem("section");
const article1 = /* #__PURE__ */ Elem("article");
const test = user => section1([
  h11($Html("Text", "Posts for " + user)),
  article1([h21($Html("Text", "The first post")), p1([$Html("Text", "This is the first post."), $Html("Text", "Not much else to say.")])])
]);
export {$Html, Elem, Text, article1, h11, h21, p1, section1, test};

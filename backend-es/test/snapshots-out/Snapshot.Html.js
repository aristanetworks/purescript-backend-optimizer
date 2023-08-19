const $Html = (tag, _1, _2) => ({tag, _1, _2});
const Elem = value0 => value1 => $Html("Elem", value0, value1);
const Text = value0 => $Html("Text", value0);
const h11$0 = /* #__PURE__ */ Elem("h1");
const h21$0 = /* #__PURE__ */ Elem("h2");
const p1 = /* #__PURE__ */ Elem("p");
const section1 = /* #__PURE__ */ Elem("section");
const article1 = /* #__PURE__ */ Elem("article");
const test = user => section1([
  h11$0([$Html("Text", "Posts for " + user)]),
  article1([h21$0([$Html("Text", "The first post")]), p1([$Html("Text", "This is the first post."), $Html("Text", "Not much else to say.")])])
]);
export {$Html, Elem, Text, article1, h11$0, h21$0, p1, section1, test};

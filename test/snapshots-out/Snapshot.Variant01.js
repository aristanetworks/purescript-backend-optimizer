import * as $runtime from "../runtime.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Partial from "../Partial/index.js";
const test3 = {type: "foo", value: 42};
const test2 = x => r => {
  if (r.type === "foo") { return Data$dShow.showIntImpl(r.value); }
  if (r.type === "bar") {
    if (r.value) { return "true"; }
    return "false";
  }
  if (r.type === "baz") { return r.value; }
  return x(r);
};
const test1 = r => {
  if (r.type === "baz") { return r.value; }
  if (r.type === "bar") {
    if (r.value) { return "true"; }
    return "false";
  }
  if (r.type === "foo") { return Data$dShow.showIntImpl(r.value); }
  return Partial._crashWith("Data.Variant: pattern match failure [" + (r.type + "]"));
};
export {test1, test2, test3};

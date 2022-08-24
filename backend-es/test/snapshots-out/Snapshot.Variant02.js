// @inline export variantBuildMatchCons arity=5
import * as $runtime from "../runtime.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dVariant from "../Data.Variant/index.js";
import * as Partial from "../Partial/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const variantBuildMatchNil = {variantBuildMatch: v => k => v1 => k};
const variantBuildMatch = dict => dict.variantBuildMatch;
const variantBuildMatchCons = dictTypeEquals => () => () => dictIsSymbol => dictVariantBuildMatch => (
  {
    variantBuildMatch: v => k => r => {
      const $8 = Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(r);
      const $9 = dictVariantBuildMatch.variantBuildMatch(Type$dProxy.Proxy)(k)(r);
      return r$1 => {
        if (r$1.type === dictIsSymbol.reflectSymbol(Type$dProxy.Proxy)) { return $8(r$1.value); }
        return $9(r$1);
      };
    }
  }
);
const match = () => dictVariantBuildMatch => dictVariantBuildMatch.variantBuildMatch(Type$dProxy.Proxy)(Data$dVariant.case_);
const test1 = r => {
  if (r.type === "bar") {
    if (r.value) { return "true"; }
    return "false";
  }
  if (r.type === "baz") { return r.value; }
  if (r.type === "foo") { return Data$dShow.showIntImpl(r.value); }
  return Partial._crashWith("Data.Variant: pattern match failure [" + (r.type + "]"));
};
export {match, test1, variantBuildMatch, variantBuildMatchCons, variantBuildMatchNil};

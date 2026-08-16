// @inline export variantBuildMatchCons arity=5
import * as $mData$dShow from "../Data.Show/index.js";
import * as $mData$dVariant from "../Data.Variant/index.js";
import * as $mPartial from "../Partial/index.js";
import * as $mRecord$dUnsafe from "../Record.Unsafe/index.js";
import * as $mType$dProxy from "../Type.Proxy/index.js";
const variantBuildMatchNil = {variantBuildMatch: v => k => v1 => k};
const variantBuildMatch = dict => dict.variantBuildMatch;
const variantBuildMatchCons = dictTypeEquals => () => () => dictIsSymbol => dictVariantBuildMatch => (
  {
    variantBuildMatch: v => k => r => {
      const $0 = $mRecord$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol($mType$dProxy.Proxy))(r);
      const $1 = dictVariantBuildMatch.variantBuildMatch($mType$dProxy.Proxy)(k)(r);
      return r$1 => {
        if (r$1.type === dictIsSymbol.reflectSymbol($mType$dProxy.Proxy)) { return $0(r$1.value); }
        return $1(r$1);
      };
    }
  }
);
const match = () => dictVariantBuildMatch => dictVariantBuildMatch.variantBuildMatch($mType$dProxy.Proxy)($mData$dVariant.case_);
const test1 = r => {
  if (r.type === "bar") {
    if (r.value) { return "true"; }
    return "false";
  }
  if (r.type === "baz") { return r.value; }
  if (r.type === "foo") { return $mData$dShow.showIntImpl(r.value); }
  return $mPartial._crashWith("Data.Variant: pattern match failure [" + r.type + "]");
};
export {match, test1, variantBuildMatch, variantBuildMatchCons, variantBuildMatchNil};

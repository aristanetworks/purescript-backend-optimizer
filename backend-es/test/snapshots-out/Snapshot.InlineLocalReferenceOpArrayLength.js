// @inline Snapshot.PrimOnLocals.a always
import * as Data$dArray from "../Data.Array/index.js";
const shouldInlineArrayLength = x => Data$dArray.snoc(x)(x.length);
const b = /* #__PURE__ */ Data$dArray.snoc([1, 2, 3])(3);
export {b, shouldInlineArrayLength};

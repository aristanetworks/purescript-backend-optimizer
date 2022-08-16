import * as $runtime from "../runtime.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Effect$dUnsafe from "../Effect.Unsafe/index.js";
const test = f => {
  const ref = Effect$dUnsafe.unsafePerformEffect(() => ({value: 0}));
  const _2 = f(ref);
  return () => {
    const _3 = _2.value;
    _2.value = 1 + _3 | 0;
    const _5 = ref.value;
    ref.value = 1 + _5 | 0;
    return Data$dUnit.unit;
  };
};
export {test};

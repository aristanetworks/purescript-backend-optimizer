import * as $runtime from "../runtime.js";
import * as Data$dUnit from "../Data.Unit/index.js";
import * as Effect$dUnsafe from "../Effect.Unsafe/index.js";
const test = f => {
  const ref = Effect$dUnsafe.unsafePerformEffect(() => ({value: 0}));
  const $2 = f(ref);
  return () => {
    const $3 = $2.value;
    $2.value = 1 + $3 | 0;
    const $5 = ref.value;
    ref.value = 1 + $5 | 0;
    return Data$dUnit.unit;
  };
};
export {test};

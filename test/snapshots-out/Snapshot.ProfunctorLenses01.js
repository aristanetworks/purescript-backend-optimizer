// @inline Data.Lens.Lens.lens arity=2
// @inline Data.Lens.Record.prop arity=4
// @inline Data.Profunctor.Strong.strongFn.first arity=1
import * as $runtime from "../runtime.js";
import * as Control$dSemigroupoid from "../Control.Semigroupoid/index.js";
import * as Data$dLens$dRecord from "../Data.Lens.Record/index.js";
import * as Data$dSemiring from "../Data.Semiring/index.js";
const test8 = a => ({...a, bar: 42 + a.bar | 0, foo: 1 + a.foo | 0});
const test7 = x => ({...x, bar: 42 + x.bar | 0, foo: 1 + x.foo | 0});
const test6 = a => ({...a, bar: {...a.bar, baz: 1 + a.bar.baz | 0}});
const test5 = x => ({...x, bar: {...x.bar, baz: 1 + x.bar.baz | 0}});
const test4 = a => ({...a, bar: 1 + a.bar | 0});
const test3 = x => ({...x, bar: 1 + x.bar | 0});
const test2 = a => a.foo;
const test1 = x => x.foo;
export {test1, test2, test3, test4, test5, test6, test7, test8};

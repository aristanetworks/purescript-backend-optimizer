# Why and How of Inline Directives

## Optimization by Evaluation

### Concept

`purs-backend-es` optimizes PureScript code by evaluating it.

For example, if `purs-backend-es` found an expression like the following:

```purs
1 + 2
```

it would evaluate that to

```purs
3
```

Below are a few other examples of the evaluations that `purs-backend-es` knows how to do. The below list is not exhaustive:

| Operation | Expression (before) | Expression (after) |
|-|-|-|
| Primitive Boolean Conjunction | `false && true` | `false` |
| Primitive String concatenation | `"hello " <> "world"` | `"hello world"` |
| Accessing a label from a literal record | `{ foo: 1 }.foo` | `1` |
| Applying a known argument to a lambda | `(\a -> a + 2) 1` | `1 + 2` |

`purs-backend-es` also knows how to optimize some case expressions.

Here's an obvious example:

```purs
-- Start: a case expression with one case row always matches.
-- So, just replace the expression `a` with its corresponding value, `1`:
case 1 of
  a -> a

-- Finish
1
```

A more useful example is **case of known constructor**:

```purs
data TwoOptions
  = Option1
  | Option2

-- Start: we know which constructor of `TwoOptions` is being `case`d on.
-- So, we can eliminate the case expression completely.
case Option1 of
  Option1 -> 1
  Option2 -> 2

-- Finish
1
```

This idea also works on data constructors that take arguments:

```purs
-- Start: we know which constructor of `Either` is being `case`d on.
-- So, we can remove the constructors from the case expression.
foo = case Right 1 of
  Left _ -> 99
  Right a -> a + 2

-- Finish
foo = case 1 of
  a -> a + 2
```

### Example

Let's see how `purs-backend-es` can optimize more complicated examples like the one below:

```purs
case Right { onePlus: (\two -> 1 + two) } of
  Left _ -> 99
  Right a -> a.onePlus 2
```

`purs-backend-es` continues to evaluate PureScript code until it detects that no other optimizations are possible. Here's how it would evaluate the above example, step-by-step:

```purs
-- Start
case Right { onePlus: (\two -> 1 + two) } of
  Left _ -> 99
  Right a -> a.onePlus 2

-- Step 1: use case-of-known-constructor
case { onePlus: (\two -> 1 + two) } of
  a -> a.onePlus 2

-- Step 2: Remove the case expression due to it having only one case row
{ onePlus: (\two -> 1 + two) }.onePlus 2

-- Step 3: Remove the record due to known label, `onePlus`.
(\two -> 1 + two) 2

-- Step 4: Remove the lambda due to applying an argument to it
1 + 2

-- Step 5: Evaluate the primitive addition: `1 + 2` equals `3`
3
```

## The Problem of Free Variables

### Example: Hidden Expressions

An expression like `1 + 2` can be optimized because we know what each of the values (i.e. `1` and `2`) are. A problem arises when an expression includes free variables. For example, `a` is a free variable in every expression below:

```purs
a + 2

a.foo

case a of
  Left _ -> 99
  Right _ -> 4
```

What is `a`? What expression is represented by it? Because `purs-backend-es` cannot know what `a` is, it cannot evaluate the code.

### Unhiding Free Variables via Inlining

However, consider the following expression where `a` _could_ be known:

```purs
a = 1
b = a + 2
```

Because `purs-backend-es` cannot see the literal `Int` value represented by `a`, it cannot evaluated `a + 2` to `3`. To solve this problem, we need to make the value represented by `a` visible. The solution is inlining.

**Inlining works by duplicating code.** Here's what our code would look like before and after inlining `a`:

```purs
-- Start
a = 1
b = a + 2

-- Finish
a = 1
b = 1 + 2
```

The `a` binding still exists after inlining, but now the `a` free variable has been removed from the expression bound by `b`. This means `purs-backend-es` can evaluate the resulting expression `1 + 2` to `3`. Here's the final before and after:

```purs
-- Start
a = 1
b = a + 2

-- Step 1: Inline the `a` free variable with its bound value `1`
a = 1
b = 1 + 2

-- Step 2: Evaluate `1 + 2` to `3`
a = 1
b = 3

-- Finish
a = 1
b = 3
```

### FFI: The Unhideable Free Variable

We know from a previous section that the expression bound by `b` below can be evaluated to `3`:

```purs
-- Start
a = 1
b = a + 2

-- Finish
a = 1
b = 3
```

However, what happens when `a` is a value defined via FFI?

```purs
foreign import a :: Int

b = a + 2
```

While we can inline `a` into `b`'s expression, it doesn't reveal the value represented by `a`. Unless `purs-backend-es` knows the implementation for `a` internally, no evaluation will occur.

**In short, inlining doesn't always trigger code optimizations.**

## The Risks of Inlining

In actuality, inlining can negatively affect the outputted code. Inlining comes with the following risks.

### Inlining Risk 1: Code Bloat

Consider this example

```purs
a = [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
r = Array.length $ Array.filter isRed a
o = Array.length $ Array.filter isOrange a
y = Array.length $ Array.filter isYellow a
g = Array.length $ Array.filter isGreen a
b = Array.length $ Array.filter isBlue a
p = Array.length $ Array.filter isPurple a
```

What happens if we inline `a`? The same array will be declared 6 times more than the original code:

```purs
a = [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
r = Array.length $ Array.filter isRed [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
o = Array.length $ Array.filter isOrange [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
y = Array.length $ Array.filter isYellow [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
g = Array.length $ Array.filter isGreen [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
b = Array.length $ Array.filter isBlue [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
p = Array.length $ Array.filter isPurple [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17 ]
```

The code will still behave the same, but the code has been bloated.

**In short, inlining "large" expressions will negatively impact the bundle or binary size of your code.**

### Inlining Risk 2: Slower Code

Let's consider another example. `getLazyInt` is an expensive computation. So, we only want to compute it once and only when necessary. Moreover, `getLazyInt` is a thunk defined in FFI. Despite forcing the thunk via a `unit` arg, `purs-backend-es` cannot evaluate it.

```purs
foreign import getLazyInt :: Unit -> Int

a = getLazyInt unit
b = 3 + a
c = 4 + a
d = 5 + a
```

So, what happens if we inlined `a` into `b`, `c`, and `d`'s corresponding expressions? Because inlining works by duplicating, we would produce the following code:

```purs
foreign import getLazyInt :: Unit -> Int

a = getLazyInt unit
b = 3 + getLazyInt unit
c = 4 + getLazyInt unit
d = 5 + getLazyInt unit
```

In other words, we're evaluating an expensive thunk 4 times. Previously, we only evaluated it 1 time. This is an example where inlining can be harmful, not beneficial.

**In short, inlining can negatively affect a program's performance.**

## Inline Directives: Guaranteeing Whether and When to Inline

We can mitigate inlining risks by controlling whether and when an expression gets inlined. While `purs-backend-es` will analyze expressions to determine whether to inline an expression, such heuristics aren't always the best for one's particular codebase.

Thus, inline directives provide guarantees about whether or not something will be inlined and when. There are three possible directives:

- `never`: an expression is never inlined, whether or not arguments are passed to it.
- `always`: an expression is always inlined, whether or not arguments are passed to it.
- `arity=x`: a function expression is only inlined once `x`-many arguments have been applied to it.

Let's say we have the following expressions:

```purs
a arg1 arg2 arg3 = arg1 + arg2 + arg3

b = a
c = a x1
d = a x1 x2
e = a x1 x2 x3
```

`x<directive>` indicates what `x` would be if we inlined `a` using `directive`. As you look through these examples, consider whether they would be desirable if `x1`, `x2`, and `x3` were small/large and strict/lazy. Note: `xNever` corresponds to the original expression as though inlining did not occur:

```purs
-- Start
a arg1 arg2 arg3 = arg1 + arg2 + arg3

bNever = a
bAlways = (\arg1 arg2 arg3 -> arg1 + arg2 + arg3)
bArity1 = a
bArity2 = a

cNever = a x1
cAlways = (\arg2 arg3 -> x1 + arg2 + arg3)
cArity1 = (\arg2 arg3 -> x1 + arg2 + arg3)
cArity2 = a x1                             -- because at least 2 args need to be applied,
                                           -- this does not get inlined

dNever = a x1 x2
dAlways = (\arg3 -> x1 + x2 + arg3)
dArity1 = (\arg3 -> x1 + x2 + arg3)        -- because inlining produces this expression...
                                           --   `(\arg2 arg3 -> x1 + arg2 + arg3) x2`
                                           -- which is then evaluated to
                                           --   `(\arg3 -> x1 + x2 + arg3)`
dArity2 = (\arg3 -> x1 + x2 + arg3)

eNever = a x1 x2 x3
eAlways = x1 + x2 + x3
eArity1 = x1 + x2 + x3
eArity2 = x1 + x2 + x3
```

**When in doubt, use the `arity=x` directive.** While there are times when using `never` or `always` is best, these situations are very rare.

## Benefits From Inline Directives Should be Verified

Let's say we have a function expression that is bound by `foo`.

```purs
foo arg1 arg2 ... = ... -- some expression
```

`foo` may be used multiple times throughout a codebase. Each case might call `foo` with a different number of arguments. Some cases may use arguments that often trigger optimizations (e.g. literal values) whereas others use free variables. As a result, it can be difficult to determine the following:

- whether or not adding an inline directive is desirable (i.e. the inline heuristics of `purs-backend-es` may be "good enough" already)
- if an inline directive should be added, which inline directive produces the best tradeoff amongst all usages of `foo`
- whether adding an inline directive on other expressions are needed to produce a better tradeoff among usages of `foo`

There's a 4-step process for answering all of these questions:

1. Define a small snippet of code
2. See what the optimized output of that code is
3. If the output isn't "good enough", add an inline directive targeting the outermost part of the code
4. Go to step 2

Using a golden test to verify this is ideal. Over time, the code may change in various ways. If a change somehow interferes with a optimization that previously worked, the corresponding inline directive (if any) may also need to be changed to still produce optimal code.

## Inline Directives: Tradeoffs Are Subjective

Let's say Codebase A exists at Company A, and Codebase B exists at Company B.

Inlining `foo` from the previous section using the inline directive `arity=3` may produce six different scenarios based on two variables:

- Variable 1: how inlining affects a codebase:
    - better code
    - worse code
    - no difference
- Variable 2: which codebase is affected
    - Codebase A
    - Codebase B

For example, inlining may produce (variable 1) better code for (variable 2) Codebase A but (variable 1) no difference for (variable 2) Codebase B. Similarly, it may produce (variable 1) worse code for (variable 2) Codebase A and (variable 1) better code for (variable 2) Codebase B.

For a particular usage of an expression (e.g. `foo`), context determines whether a given inline directive for that expression effectively mitigates or directly causes inlining risks.

Thus, `purs-backend-es` provides different levels of control over inline directives and what code each one affects. The table below is the same one in the README but highlights who likely defines a directive at that level.

| Location | Affects | User |
|----------|---------|------|
| Module A's header, `@inline` module B directive | Module B's usages in module A | App developer's usage of some library "in particular" |
| Directives file | All modules | Application developer's usage "in general"
| Module A's header, `@inline export` module A directive | Module A's usages in all modules | Library developer's recommended inline directives |
| Default heuristics | All modules | `purs-backend-es` default analysis |

## Summary

`purs-backend-es` optimizes PureScript code by evaluating specific known expressions. Free variables inside of such expressions can hinder its ability to evaluate those expressions. Fortunately, inlining the expressions represented by free variables can trigger optimizations that were otherwise not apparent.

Unfortunately, inlining such expressions comes with two risks: code bloat and slower code. One can mitigate such risks by using inline directives. Inline directives guarantee whether an expression is inlined and under what circumstances.

Moreover, inlining the same expression can impact different codebases differently, both positively and negatively. To mitigate such risks while still getting optimal code, one can configure inline directives at various levels.

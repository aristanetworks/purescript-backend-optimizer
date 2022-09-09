# Why and How of Inline Directives

This page explains four things:

1. The primary optimization `purs-backend-es` does
2. How inlining and inline directives help rewrite rules to optimize code
3. The most cost-efficient methodology for defining inline directives that are useful
4. Guiding principles as to where to define such inline directives

## The Goal of Optimizations

What's the difference between the following two expressions?

```purs
case1 :: forall a. FailureOrSuccess a -> Boolean
case1 someComputation =
  maybeToBoolean $ eitherToMaybe $ resultToEither someComputation
  where
  resultToEither :: FailureOrSuccess a -> Either String a
  resultToEither arg = case  of
    Failure -> Left "There was an error!"
    Success a -> Right a

  eitherToMaybe :: Either String a -> Maybe a
  eitherToMaybe arg = case arg of
    Left _ -> Nothing
    Right a -> Just a

  maybeToBoolean :: Maybe a -> Boolean
  maybeToBoolean arg = case arg of
    Nothing -> false
    Just a -> true

case2 :: forall a. FailureOrSuccess a -> Boolean
case2 someComputation = case someComputation of
  Failure -> false
  Success _ -> true
```

While a developer would never intentionally write an expression like `case1`, such an expression may still arise if one has a long enough "chain" of functions spread across different modules.

<details>
<summary>For an example of such a "chain", expand this accordion</summary>

```purs
-- Module1.purs
someComputation :: forall a. Int -> FailureOrSuccess a

-- Module2.purs
resultToEither :: FailureOrSuccess a -> Either String a
resultToEither arg = case  of
  Failure -> Left "There was an error!"
  Success a -> Right a

computationAsEither = resultToEither $ someComputation 4

-- Module3.purs
eitherToMaybe :: Either String a -> Maybe a
eitherToMaybe arg = case arg of
  Left _ -> Nothing
  Right a -> Just a

computationAsMaybe = map doSomethingElse computationAsEither

-- Module4.purs
maybeToBoolean :: Maybe a -> Boolean
maybeToBoolean arg = case arg of
  Nothing -> false
  Just a -> true

-- Module5.purs
foo = do
  let x = maybeToBoolean computationAsMaybe
  ...
```

</details>

At the end of the day, evaluating `case1` and `case2` produce the same boolean value, but `case1` produces and then consumes 2 intermediate data structures unnecessarily: `Either` and `Maybe`.

**To summarize, the goal of our optimizations is to remove these unneeded intermediate data structures.** As a result, the code is more performant because there's less purely-overhead work for the computer to do at runtime.

## How Code Gets Optimized

### Primitive Data Flow: Production and Consumption

How does `purs-backend-es` know when unneeded intermediate data structures are being used and when it is safe to remove them? These usages arise when primitive data is "produced" and then immediately "consumed".

For example, the `Right` data constructor below is "produced" when it wraps `"value`" and then immediately consumed by `eitherToMaybe`.

```purs
eitherToMaybe :: forall l r. Either l r -> Maybe r
eitherToMaybe = case _ of
  Left _ -> Nothing
  Right a -> Just a

eitherToMaybe $ Right "value"
```

Here's another example using records. The `{ bar: 42 }` record is "produced" by being defined in a let binding. It is then immediately "consumed" when its `bar` field is accessed. Thus, the record can be removed entirely and `a.bar` can be replaced with `42`.

```purs
foo = do
  let
    a = { bar: 42 }
    b = 1 + a.bar
  b
```

Lastly, here's an example with a few different conclusions:

- If we do NOT know what `f` will do with `a` (e.g. if `f` was some FFI function), then we cannot call the record an "unnecessary intermediate data structure." `f` may need the entire record, and that possibility forces us to call the record structure necessary.
- If we do know what `f` will do (e.g. if `f = _.bar`), then this is still an example of a primitive data "production" followed by immediate primitive data "consumption".

```purs
foo = do
  let
    a = { bar: 42, baz: "something" }
    b = 1 + f a
  b
```

**To summarize, a data producer followed immediately by a data consumer indicates a place where an unneeded intermediate data structure exists and can be removed.**

### Removing Unneeded Data Structures

#### Inlining and Inline Directives

An inliner replaces a function's call site (e.g. stuff on the left-hand-side of the `=`) with its implementation (e.g. stuff on the right-hand-side of the `=`). This replacement means the function's body is duplicated and will appear at least twice in the resulting code: once in its original definition and once in the usage site.

Here's one example using the function `binaryPlus`:

```purs
binaryPlus :: Int -> Int -> Int
binaryPlus a b = a + b

usage = 9 + binaryPlus 1 2
```

There's a few ways we could inline the expression, `binaryPlus 1 2`. While `purs-backend-es` will performance its own analysis to determine whether and when to inline it, it doesn't know as much as the developer does.

A developer can use **inline directives** to tell `purs-backend-es` whether to inline a function's body to its usage site and when (i.e. how many args need to be passed to the function before the inlining occurs):

- never inline it at all (i.e. the directive is `never`)
    - `9 + binaryPlus 1 2`
- always inline it with no consideration for the number of arguments applied to it (i.e. the directive is `always`)
    - `9 + (\a b -> a + b) 1 2`
- inline it only after at least one argument has been applied to it (i.e. the directive is `arity=1`)
    - `9 + (\  b -> 1 + b)   2`
- inline it only after at least two arguments have been applied to it (i.e. the directive is `arity=2`)
    - `9 + (        1 + 2)`

**To summarize, inline directives provide developers with a stronger guarantee about whether and when a function is inlined than just relying upon the analysis done by `purs-backend-es`.**

#### Inlining Duplicates Code

The below example highlights the risk of inlining everything without thought: pointless code duplication.

```purs
hasExpensiveLetBinding a b = do
  let
    someValue = expensiveComputation
  1 + someValue * b + a

usage1 = hasExpensiveLetBinding 1 2
usage2 = hasExpensiveLetBinding 3 4
```

If we inline `hasExpensiveLetBinding` immediately, the `expensiveComputation` will be computed three times rather than once. In other words, it would be the same as writing the following in source code:

```purs
hasExpensiveLetBinding a b = do
  let
    someValue = expensiveComputation
  1 + someValue * b + a

usage1 = (\a b -> do
    let
      someValue = expensiveComputation
    1 + someValue * b + a
  ) 1 2
usage2 = (\a b -> do
    let
      someValue = expensiveComputation
    1 + someValue * b + a
  ) 3 4
```

**To summarize, inlining for the sake of inlining is a great way to unnecessarily increase your program's bundle size**.

#### Rewrite Rules

A **rewrite rule** will replace one expression, usually involving literal values, with an equivalent expression. For example, an expression like `1 + 2` will always produce the value `3`. So, we can replace that expression with `3`. The rewrite rule here could be defined as, "Whenever the `+` function is called on two literal `Int` values, replace that expression with the sum of the two `Int` values."

```purs
-- Before
foo = 1 + 2
  --  ^^^^^ Rewrite rule: "I can optimize that! `1 + 2` is `3`"

-- After
foo = 3
```

However, some expressions will prevent such rewrite rules from triggering. For example:

```purs
foo =
  let
    a = { bar: 42 }
  in
    1 + a.bar
--  ^^^^^^^^^ Rewrite rule: "I can't optimize that! `a.bar` is not a literal `Int` value."
```

Because the rewrite rule cannot see the literal `Int` value represented by `a.bar`, its optimization does not trigger. To solve this problem, we need to make it visible. The solution is to duplicate code via inlining.

**To summarize, inlining duplicates code so that rewrite rules can trigger optimizations that were otherwise hidden from its eyes.**

#### Code Optimzation Example via Rewrite Rules and Inlining

Let's see how this works in practice using this example:

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = ignoreArgs 8 9
```

Let's assume `ignoreArgs` has an inline directive of `arity=1`. Here's what happens when we optimize this code piece.

First, we see that `ignoreArgs` has 2 arguments applied. Since `2` >= `1`, the arity of the directive we specified, we inline `ignoreArgs`. Since `8` corresponds to `arg1`, we replace `arg1` with `8` when inlining `ignoreArgs`. We don't remove the original `ignoreArgs` declaration.

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = (\arg2 -> do
  let
    a = { bar: 41 }
  1 + a.bar + 8 + arg2
  ) 9
```

At this point, `purs-backend-es`' default inliners will see a lambda (i.e. a "producer") being immediately applied to an argument (i.e. a "consumer"). Thus, it will inline that argument into the lambda's body. This gets us:

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = do
  let
    a = { bar: 41 }
  1 + a.bar + 8 + 9
```

A rewrite rule will detect the `8 + 9` expression, and replace it with its sum: `17`. However, `1 + a.bar` and `a.bar + 17` don't trigger the rewrite rule because one of the values is not a literal `Int` value.

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = do
  let
    a = { bar: 41 }
  1 + a.bar + 17
```

Fortunately, `purs-backend-es`' default inliners will see a record binding (i.e. a "producer") that is immediately accessed under the label `bar`. Thus, the corresponding value will be inlined:

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = do
  let
    a = { bar: 41 }
  1 + 41 + 17
```

The rewrite rules then detects that `1 + 41` is `42`:

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = do
  let
    a = { bar: 41 }
  42 + 17
```

The rewrite rules then detects that `42 + 17` is `59`:

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = do
  let
    a = { bar: 41 }
  59
```

Lastly, `purs-backend-es` determines that the `a` binding is never used; thus, it's removed entirely:

```purs
ignoreArgs arg1 arg2 = do
  let
    a = { bar: 41 }
  1 + a.bar + arg1 + arg2

foo = 59
```

Now that we've finished optimizing `foo`, the following states are true:

- `ignoreArgs` is still defined as it originally was
- the optimized `foo` still represents the same value as the unoptimized `foo` would have had at runtime.

## A Methodology for Defining Inline Directives

Inlining duplicates code so that rewrite rules can trigger optimizations that were otherwise hidden from its eyes. Ideally, inlining values will always trigger rewrite rules that both reduce the bundle size of the code AND make the resulting program more performant. However, inlining may increase a program's bundle size without improving its performance.

In short, this process isn't scientific. While one can add inline directives without much thought, the result likely won't be what they want.

So, how should one determine if an inline directive needs to be added? One should always use the below methodology:

1. Think of a snippet of code you want to ensure is optimized.
2. Define a snapshot for that snippet.
3. Look at the current JavaScript output of that snapshot.
4. If the current output is not good enough,
    1. add an inline directive to the outermost thing
       1. When in doubt, use `arity=x` where `x` is the number of args the function in the outputted JavaScript, not in PureScript source code, takes.
    2. Go to Step 3.
5. Once the output satisfies you, determine where it should be defined (see next section)

First, think of a snippet of code you want to ensure is optimized. If you don't have a goal in mind, you will add inline directives that will unnecessarily bloat your code.

Second, define a snapshot for that snippet. Without a small snippet of code, you won't be able to see what affects adding more directives may have.

Third, look at the current JavaScript output of that snapshot. If it's already as optimized, there's no reason to add an inline directive. You're done.

Otherwise, fourth, find the outermost function and add an inline directive for that. Because `purs-backend-es` operates on `CoreFn`, not PureScript source code, refer to the function in the outputted JavaScript to determine how many args the function takes.

### Where to Put Inline Directives

Because inline directives affect the size/performance tradeoff of the code, one tradeoff produced by an inline directive may be desirable to one person but unacceptable to another person. `purs-backend-es` provides default inline directives for a number of the `core` libraries because these will probably be desirable to all users regardless of their purpose. Anything outside of that is debatable. That's why the `--directives` flag exists.

| Location | Affects | User |
|----------|---------|------|
| Module A's header, `@inline` module B directive | Module B's usages in module A | App developer's usage of some library in particular |
| Directives file | All modules | Application developer's usage in general
| Module A's header, `@inline export` module A directive | Module A's usages in all modules | Library developer's recommended inline directives |
| Default heuristics | All modules | Defaults |
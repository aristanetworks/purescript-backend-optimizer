# Optimized Pattern Matching

The PR that implemented optimized pattern matching was initially based on https://julesjacobs.com/notes/patternmatching/patternmatching.pdf (herein referenced by the "Jacobs' paper"). The "Jacobs' paper" summarized and provided a simple but less-optimized algorithm based on https://www.cs.tufts.edu/comp/150FP/archive/luc-maranget/jun08.pdf (herein referenced by the "Compiling Pattern Matching to Good Decision Trees" paper or CPMtGDT paper). As time went on, the PR was updated to use more and more ideas from CPMtGDT. In this project, we use heuristic `pbaN` (explained further below) from the CPMtGDT paper to determine the next column to test.

See the module header comment in [Convert.purs](./src/PureScript/Backend/Optimizer/Convert.purs) for a summary of the algorithm used. The rest of this page provides context behind why that algorithm works that way. This page summarizes the two aforementioned papers before covering some things specific to our needs here.

## Non-PureScript-Specific Things

### Overview of General Algorithm

#### Clause Matrix

A typical `case _, _, _ of` expression can be represented as a **clause matrix** with additional information stored in the rows. A **clause matrix** is an array of case rows that each have the same number of columns, which correspond to the value against which we are pattern matching across all rows for that column. For example, rather than writing this...
```
case a, b, c of
  1, 2, 3 -> "foo"
  2, _, 3 -> "bar"
  _, _, _ -> "baz"
```
... one could write this using the `value is pattern` syntax mentioned in the Jacobs' paper:
```
a is 1, b is 2, c is 3 -> "foo"
a is 2, b is _, c is 3 -> "bar"
a is _, b is _, c is _ -> "baz"
```
... and visualize it as a "matrix" of rows and columns where the rows contain additional information (i.e. the case row expression).

|  | Pattern Match<br />Column 0 | Pattern Match<br />Column 1 | Pattern Match<br />Column 2 | Expression
|-|-|-|-|-|
| Case Row 0 | a is 1 | b is 2 | c is 3 | "foo"
| Case Row 1 | a is 2 | b is _ | c is 3 | "bar"
| Case Row 2 | a is _ | b is _ | c is _ | "baz"

#### Implementing Optimized Pattern Matching

To produce an optimized pattern match, we generate a tree. The branches of the tree are tests whereas the leafs are the case row expressions. There are two parts to implementing an optimized pattern match:
1. only test a `value is pattern` test once in a given tree path from root to leaf. For example, the above clause matrix has 4 tests:
		- `a is 1`
		- `a is 2`
		- `b is 2`
		- `c is 3`
	Each leaf's path from the root should only include the corresponding tests just once.
2. test the column whose patterns most affect the size of the tree. Put differently, one tree is better than another tree if the former requires less branches than the latter to implement the same pattern match. The difference in tree sizes is often caused by which column is tested.
For example, if we were to generate the pattern matching for the above code in JavaScript, there are two trees we could consider to highlight this point.

First, here's a version of the above pattern match where the `c is 3` test occurs twice in the below code:
```js
if (a === 1) {
  if (b === 2) {
    if (c === 3) { // once
      return "foo";
    }
  }
}
if (a === 2) {
  if (c === 3) { // twice
    return "bar";
  }
}
return "baz";
```
Here's a more optimized version of the same pattern match where the corresponding `c is 3` test only occurs once:
```js
if (c === 3) { // only once
  if (a === 1) {
    if (b === 2) {
      return "foo";
    }
  }
  if (a === 2) {
    return "bar";
  }
}
return "baz";
```

#### Using Clause Matrices to build optimized trees

To generate these threes, we use an algorithm that works on a clause matrix. This algorithm follows the general idea described in the Jacobs' paper but aligns more in spirit with the CPMtGDT paper. See the bottom of this section for differences:

1. If the clause matrix has 0 rows, then we produce a pattern match failure
2. Otherwise, there's at least 1 row. If the clause matrix's first row contains only wildcard patterns (e.g. `value is _`), then we produce the case row's expression
3. Otherwise, there's at least one column in the first row against which we still need to test (i.e. there is a `value is pattern` test where the `pattern` is not a wildcard/`_`).
    1. From among the remaining non-wildcard patterns we could test, use a heuristic to determine which column's `value is pattern` test from the first row will produce the smallest tree
    2. Build 2 new clause matrices, Problem A and Problem B, using the below rules and then recurse. Problem A contains rows where a match occurred. Problem B contains rows where a match did not occur.
        1. If a row's corresponding column uses the same pattern as the chosen one (e.g. `chosen: a is 1; row's: a is 1`), then remove that pattern from the array of patterns and put the result in Problem A because a match occurred.
        2. If a row's corresponding column differs from the chosen one (e.g. `chosen: a is 1; row's: a is 2`), then put it in Problem B; a match did not occur.
        3. If a row's corresponding column is a wildcard (e.g. `chosen: a is 1; row's: a is _`) then put a version of the row using the same instructions described above in Problem A and put an unmodified version of the row in Problem B.

**Notably, in step 3.2.3, duplicating this row causes one tree's size to differ from another tree's size**. This is why the column next tested is important.

The resulting code would look something like this in JavaScript:
```js
if (chosenColumnTest) {
  <problemACodegen>
}
<problemBCodegen>
```

Differences from the Jacobs' paper:
- The Jacobs' paper produces the case row expression when there are 0 columns left to test. This implementation produces the case row expression when there are 0 columns left to test OR all the remaining columns are wildcards. This is done to solve the "reference problem" described later.

### Adding Support for Subterms

What happens if we want to support pattern matching on things that may contain subterms? For example:
```
case a of
	Add 1 2 -> "1+2"
	Add 3 4 -> "3+4"
	_       -> "???"
```
Reformulating the first row to `a is Add 1 2` would be incorrect as we're not testing on the full expression but only the `Add` wrapper. Rather, we should reformulate it into two parts:
1. what is being tested
2. what new patterns are introduced to the matrix if the test succeeds

Thus, we'll update our clause matrix to use a new syntax: `a is Add; subterms [a._0 is 1, a._1 is 2]`. The `subterms [a._0 is 1, a._1 is 2]` part clarifies that once the initial test succeeds, we will replace the original `value is pattern` with the subterms' patterns.

As for the wildcard, we'll say it `subterms _` to indicate that it will always introduce as many wildcards as non-wildcard patterns would introduce. This ensures that the number of columns remains constant across all case rows.

Thus, using our updated clause matrix idea, we would reformulate the above idea as

|  | Pattern Match Column 0 | Expression
|-|-|-|
| Case Row 0 | a is Add<br />subterms [a._0 is 1, a._1 is 2] | "1+2"
| Case Row 1 | a is Add<br />subterms [a._0 is 3, a._1 is 4] | "3+4"
| Case Row 2 | a is _<br />subterms _ | "???"

Following the algorithm described above, we would use the `a is Add` test. The difference is in Step 3.2.1 and Step 3.2.3. Rather than removing the column completely, we replace the original pattern with its subterm patterns. This gets us the following two clause matricies. Notice how the number of columns has increased:

(Problem A)
|  | Pattern Match<br />Column 0 | Pattern Match<br />Column 1 | Expression
|-|-|-|-|
| Case Row 0 | a._0 is 1<br />subterms [] | a._0 is 2<br />subterms [] | "1+2" |
| Case Row 1 | a._0 is _<br />subterms _ | a._0 is _<br />subterms _ | "???" |

(Problem B)
|  | Pattern Match<br />Column 0 | Pattern Match<br />Column 1 | Expression
|-|-|-|-|
| Case Row 0 | a._0 is 3<br />subterms [] | a._0 is 4<br />subterms [] | "3+4" |
| Case Row 1 | a._0 is _<br />subterms _ | a._0 is _<br />subterms _ | "???" |

### The Column Choosing Heuristic

Per the CPMtGDT paper, they conducted experiments to see what is the best heuristic to use for producing the best tree. They concluded that using the composition of heuristics, `p`, `b`, and `a`, led to the best pattern matching tree.

The Jacobs' paper uses the heuristic `n`, which is different from pseudo-heuristic `N`. This project uses heuristic `pbaN`, composing the best heursitic found `pba` with the tie-breaker heuristic `N`. Each is summarized below and then further described in their corresponding section.

The computation of the heuristic scores are designed such that the highest score wins. In some situations, the best score is the lowest one, so it seems the authors negate the scores, so that the 'lowest' scores are still the highest value.

- Heuristic `p`'s goal: determine which column has the largest impact on the rest of the tree by determining which column is "needed" sooner than the others.
- Heuristic `b`'s goal: get rid of columns ASAP by finding the column with the least remaining <strong>immediate</strong> tests.
- Heuristic `a`'s goal: get rid of columns ASAP by finding the column with the least remaining <strong>future</strong> tests via one lookahead.
- Pseduo-herusitic `N`'s goal: break ties among the columns remaining at this point by using the left-most column.

#### Heuristic `p`

```purs
case a, b, c of
  1, 2, 1 -> 1
  1, 2, 2 -> 2
  _, 2, 3 -> 3
  1, _, 4 -> 4
  _, _, _ -> 5
```

Heuristic `p` is calculated by finding the maximum row index, `score` such that `p` is needed for all row indices 0 to `score`. In the above example, the scores would be:

- `a`: 1 (rows 0-1)
- `b`: 2 (rows 0-2)
- `c`: 0 (row 0)

Even though columns `a` and `b` match their corresponding values (i.e. `1` and `2`, respectively) the same number of times, column `b`'s 3rd usage is needed sooner than column `a`'s 3rd usage, so it impacts the decision tree more.

#### Heuristics `b` and `a`

Given this example:

```purs
case a, b of
  1, 1 -> 1
  _, 2 -> 2
  _, 3 -> 3
  _, _ -> 4
```

If we pattern match on `a is 1`, we can get rid of the `a` column immediately. If we match on the `b is value` column first, we'll need to test `a is 1` for each tree generated by the three `b is value` tests. So, we want to eliminate the `a is 1` test as soon as possible.

There are two ways we can do that, which is covered by the remaining heuristics.

##### Heuristic `b`


```purs
case a, b of
  Ctor1 x, Ctor1 x -> 1
  Ctor2 x, Ctor2 x -> 2
  _      , Ctor3 y -> 3
  _      , _       -> 4
```

The heuristic `p` score above is `0` for both tests because the columns are the same. Thus, we must use a different heuristic.

Heuristic `b` is calculated by creating a set consisting of all the non-`PatWild` ctors in a given column and then calculating its size (i.e. cardinality). In this case, the best score is the smallest number, so we make the smallest number the largest number by negating it.
CPMtGDT mentions subtracting one from the result if we don't have a complete signature. I believe that translates to "not having an exhaustive pattern match." If correct, then we can ignore that for our purposes.

Thus, the heuristic `b` score for each column above is:

- `a`: -2
- `b`: -3

And we'd go with column `a` because we can get rid of it sooner.

##### Heuristic `a`


```purs
case a, b of
  One 1  , One 1   -> 1
  One 2  , One 2   -> 2
  One 3  , One 3   -> 3
  One 4  , One 4   -> 4
  Two 1 _, Two _ _ -> 5
  _      , _       -> 6
```

The above two columns have the same heuristic `p` and `b` scores, `0` and `-5` respectively.

Heuristic `a` is calculated by adding the size of the non-`PatWild` subterms (i.e the aritiy) of each non-`PatWild` ctor in a given column. Again, the best score is the smallest number, so we make the smallest number the largest number by negating it.

Thus, the heuristic `a` score for each column above is:
- `a`: -5 (`negate $ (1 subterm * 4 "One" ctors) + (1 subterm * 1 "Two" ctor`)
- `b`: -4 (`negate $ (1 subterm * 4 "One" ctors) + (0 subterms * 1 "Two" ctor`)

And we'd go with column `b` because we can get rid of column `b` sooner than column `a`.

#### Pseudo-Heuristic `N`

```purs
case a, b of
  One 1  , One 1   -> 1
  One 2  , One 2   -> 2
  One 3  , One 3   -> 3
  One 4  , One 4   -> 4
  Two 1 _, Two 2 _ -> 5
  _      , _       -> 6
```

The above two columns have the same herustic `p`, `b`, and `a` scores: `0`, `-5`, and `-5` respectively. At this point, we still have 2 candidates. To break this tie, we just use the left-most one. In this case, we would use column `a`.

### The Reference Problem

#### Explaining the problem

Thus far, our pattern matching examples have never introduced references via the binders. For example, `x` and `y` in the below expression:
```
case a, b of
  x, Add y 2 -> x + y
  _, _ -> 2
```

`x` and `y` introduce references that can be used in later expressions. However, they function the same as wildcards/`_` in a pattern match. So, a pattern like `a is referenceName` should still be represented as `a is _` but the references they introduce need to be handled separately.

To account for this change, we want to update our clause matrix to additional track two things:
1. for an individual `value is pattern`, what references it introduces if it tests successfully.
    - we will use the syntax `introduces [reference1=value1, ..., referenceN=valueN]` for this (in case multiple references are introduced)
2. for a case row, what references have been introduced thus far in the algorithm due to its recursive nature
    - we will use the syntax `reference=value` and add this "additional information" to each case row

Reformulating the above expression as a clause matrix, we now get:

|  | Pattern Match<br />Column 0 | Pattern Match<br />Column 1 | References | Expression
|-|-|-|-|-|
| Case Row 0 | a is _<br/>introduces [x=a]<br/>subterms [] | b is Add<br/>introduces []<br/>subterms<br/>[ a._0 is _; introduces [y=a._0]; subterms []<br/>, a._1 is 2; introduces []; subterms [];<br/>] | [] | x + y |
| Case Row 1 | a is _<br/>introduces []<br/>subterms _ | b is _<br/>introduces []<br/>subterms _ | [] | 2 |

Similar to before, in Step 3.2.1 and Step 3.2.3, we move the references stored in a column's `introduces` array into the case row's References array. For example, if we chose to pattern match on column 0 (i.e. the `a is _` pattern), the resulting Problem A and B matrices would be:

|  | Pattern Match<br />Column 0 | References | Expression
|-|-|-|-|
| Case Row 0 | b is Add<br/>introduces []<br/>subterms<br/>[ a._0 is _; introduces [y=a._0]; subterms []<br/>, a._1 is 2; introduces []; subterms [];<br/>] | [x=a] | x + y |
| Case Row 1 | b is _<br/>introduces []<br/>subterms _ | [] | 2 |

When we finally get to a clause matrix where all the rows are wildcards, some of these columns may introduce references. For example:
|  | Pattern Match<br />Column 0 | Pattern Match<br />Column 1 | References | Expression
|-|-|-|-|-|
| Case Row 0 | x is _<br/>introduces [d=a._0._1]<br/>subterms [] | y is _<br/>introduces [e=a._0._2]<br/>subterms [] | [b=a._1, c=a._2] | b + c + d + e |

At this point, the references introduced by binders in a case row are stored in two places and must be recombined before we have all references in one array:
1. the "References" array stored in the case row (i.e. `[b=a._1, c=a._2]`)
2. the `introduces` values for each column (i.e. `d=a._0._1` and `e=a._0._2`.

Thus, we must combine the references in the case row with all added by the remaining columns. We'll refer to the result as `allReferences`.

#### Solving the Problem

Once that's done, there are two ways we can ensure that the references in `allReferences` are in scope when the case row's expression executes. Each has an example following it below:
1. We introduce the references into the current scope and execute the expression
2. We create a function that takes as many arguments as the number of references introduced by the case row's binders and its body is the case row's expression. We then store a reference to that function. When it's time to produce the case row expression in Step 2, we call the function with the references' values from `allReferences`.

(Example of approach 1: the Jacobs' Paper)
```js
if (b.tag === "Add") {
  const x = a;
  const y = b._0;
  return x + y; // case row expression
}
```

(Example of approach 2: the function approach)
```js
const row1Expression = (x, y) => {
  return x + y; // case row expression
}
if (b.tag === "Add") {
  return row1Expression(a, b._0);
}
```

The first solution is used in the Jacob's paper. It's implemented as a preprocessing step: all references are moved to the case row's expression before the algorithm begins. While this is easier to implement, it comes at the cost of duplicate code. If the case row's expression is "large", that expression will appear every time that leaf appears (e.g. rows with a wildcard pattern match). For JavaScript code, this will increase the bundle sizes. For other backends, this would increase the binary size.

The second solution is used in this project. Due to the inliner, the function call may be re-inlined back into the first solution if the case row expression is "small" enough. If they're too big, then they shall remain as function calls, decreasing the size of the resulting code.

### Summary of Algorithm So Far

1. If the clause matrix has 0 rows, then we produce a pattern match failure
2. Otherwise, there's at least 1 row. If the clause matrix's first row contains only wildcard patterns (e.g. `value is _`), then
    1. calculate the `allReferences` value by combining the case row's "References" array with the references introduced by each remaining column (if any)
    1. if using the Jacobs' paper's approach
        1. add to the local scope any references introduced by binders in the case row
        2. produce the expression
    2. if using the function call approach
        1. call the function using the references in the left-to-right parent-to-subterm order of their appearance in the original case row
3. Otherwise, there's at least one column in the first row against which we still need to test (i.e. there is a `value is pattern` test where the `pattern` is not a wildcard/`_`).
    1. From among the remaining non-wildcard patterns we could test, use a heuristic to determine which column's `value is pattern` test from the first row will produce the smallest tree (e.g. heuristic `p`)
    2. Build 2 new clause matrices, Problem A and Problem B, using the below rules and then recurse. Problem A contains rows where a match occurred. Problem B contains rows where a match did not occur.
        1. If a row's corresponding column uses the same pattern as the chosen one (e.g. `chosen: a is 1; row's: a is 1`), then
            1. in the case row, add the references row's corresponding pattern introduces the case rows' "References" array
            2. in the case row's columns, replace the parent pattern with its subterm patterns (if any)
            3. put the case row into Problem A because a match occurred
        2. If a row's corresponding column differs from the chosen one (e.g. `chosen: a is 1; row's: a is 2`), then put it in Problem B; a match did not occur.
        3. If a row's corresponding column is a wildcard (e.g. `chosen: a is 1; row's: a is _`)
            1. follow the instructions above as if there was a normal match and put the resulting row in Problem A
            2. put a copy of the row in Problem B

## Supporting PureScript-specific things

The rest of this thread explains how PureScript-specific things were implemented using this project's code. The important difference with our context as opposed to the above papers' contexts is that we are working on values that have already been typechecked. As a result, we can make the following assumptions:
- all references introduced by a binder in a given case row are always unique from all the other references introduced in that case row
- all subterms introduced by binders (e.g. product types, newtypes, sum types, records, and arrays) are guaranteed to have the same type

### Removing newtypes

There is no difference at runtime between a value and that same value wrapped in a newtype. Thus, when we convert a CoreFn `Binder` into a representation more conducive to the clause matrix idea (i.e. `Pattern`), we remove the Newtype wrapper entirely.

### Before doing anything else, always replace a parent pattern with its subterm patterns if that parent pattern is guaranteed to match

With newtypes removed, product types (i.e. data types with only 1 constructor), and records always successfully pattern match against their values. In other words, there's no test to make.

Thus, we use the above algorithm to produce Problem A's clause matrix and recurse only on that clause matrix. The end result is that the product type's subterms and record field's patterns are inlined into the case row's patterns. Thus, when the 'chosen column' is a product type or record, the algorithm functions more like a preprocessing step that just so happens to follow the same code path as something that does involve a test (e.g. sum types).

### Fixing Record Fields

Before we can "inline" a record's subterms (i.e its fields), we need to clean it up.

The below expression...
```purs
caseRecord :: { a :: Int, b :: Int } -> Int
caseRecord = case _ of
  { a: 1 } -> 1
  { b: 2 } -> 2
  { a, b: 3 } -> 3
  { b, a } -> 4
```
currently produces the following visualized CoreFn
```
{ a: lit 1          }
{          b: lit 2 }
{ a: var,  b: lit 3 }
{ b: var,  a: var   }
```

Two problems arise with the unprocessed representation.
1. This representation is missing information and thus cannot be converted into a clause matrix because some rows contain 2 columns whereas other rows contain 1 column. Each row must have the same number of columns.
2. If the fields are ordered differently in 2+ rows (e.g. the last two rows above), any subterm patterns replacing their parent pattern may appear in different columns. This would break a constraint of the clause matrix: each column in each case row's columns array tests against the same value as represented by that column.

To fix this first problem, we insert any missing field binders for literal record binders in each column. This is done by calculating what are all the fields used for that column in one pass and then adding the missing ones in a second pass as a wildcard pattern that does not introduce any references. This preprocessing step must be done before we can continue the algorithm in case a previous match introduced a subterm that is a record.

When the preprocessing step is finished, we should have something that looks like this:
```
case rec of
  { a: 1, b: _ } -> 1
  { a: _, b: 2 } -> 2
  { a: _, b: 3 } -> 3
  { a: _, b: _ } -> 4
```
which can be converted into a clause matrix like:

|  | Pattern Match<br />Column 0 | Pattern Match<br />Column 1 | References | Expression
|-|-|-|-|-|
| Case Row 0 | rec.a is 1<br/>introduces []<br/>subterms [] | rec.b is _<br/>introduces []<br/>subterms [] | [] | 1 |
| Case Row 1 | rec.a is _<br/>introduces []<br/>subterms [] | rec.b is 2<br/>introduces []<br/>subterms [] | [] | 2 |
| Case Row 2 | rec.a is _<br/>introduces []<br/>subterms [] | rec.b is 3<br/>introduces []<br/>subterms [] | [] | 3 |
| Case Row 3 | rec.a is _<br/>introduces []<br/>subterms [] | rec.b is _<br/>introduces []<br/>subterms [] | [] | 4 |

### Supporting Guards

What about adding support for guards? For example
```purs
case a of
  Left l
    | l == 1 -> 1
    | l == 2 -> 2
  Right r
    | r == 1 -> 3
    | r == 2 -> 4
  Left l -> 5
  Right r -> 6
```

The papers above do not clarify how to support guards, but the idea is simple: if all the guards for a case row fail, all the rows below the chosen row should be tested. In the example above, the `Left l -> 5` row should be tested if the `Left l | l == 1` and `Left l | l == 2` guards fail. In other words, the above expression is a more readable version of the below equivalent expression:
```purs
case a of
  Left l -> case l of
    1 -> 1
    2 -> 2
    _ -> 5
  _ -> case a of
    Right r -> case r of
      1 -> 3
      2 -> 4
      _ -> 6
    _ -> Impossible
```

When the case row expression is `Unconditional`, it has no guards. So, we just convert the case row expression into `BackendExpr`.
If, however, it's `Guarded`, then the case row expression has guards. Thus, if the guards all fail, we should fallback to the rest of the rows in the clause matrix.

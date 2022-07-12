module ExampleDirectives where

import Prelude

-- This is just `const` but with a bunch of args in front to illustrate directives.
inlineNever :: forall a b x1 x2 x3 x4 x5 x6. a -> b -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> a
inlineNever a _ _1 _2 _3 _4 _5 _6 = a

inlineAlways :: forall a b x1 x2 x3 x4 x5 x6. a -> b -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> a
inlineAlways a _ _1 _2 _3 _4 _5 _6 = a

inlineArity3 :: forall a b x1 x2 x3 x4 x5 x6. a -> b -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> a
inlineArity3 a _ _1 _2 _3 _4 _5 _6 = a

test1_never =
  { a01: inlineNever
  , a02: inlineNever "a"
  , a03: inlineNever "a" "b"
  , a04: inlineNever "a" "b" 1
  , a05: inlineNever "a" "b" 1 2
  , a06: inlineNever "a" "b" 1 2 3
  , a07: inlineNever "a" "b" 1 2 3 4
  , a08: inlineNever "a" "b" 1 2 3 4 5
  , a09: inlineNever "a" "b" 1 2 3 4 5 6
  , a10: inlineNever "a" "b" 1 2 3 4 5 6
  , a11: inlineNever "a" "b" 1 2 3 4 5 6
  , a12: inlineNever "a" "b" 1 2 3 4 5 6
  , a13: inlineNever "a" "b" 1 2 3 4 5 6
  , a14: inlineNever "a" "b" 1 2 3 4 5 6
  , a15: inlineNever "a" "b" 1 2 3 4 5 6
  , a16: inlineNever "a" "b" 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: inlineNever "a" "b" 1 2 3 4 5 6
  }

test1_neverRec = test1_never.a02 "b" 1 2 3 4 5

test2_always =
  { a01: inlineAlways
  , a02: inlineAlways "a"
  , a03: inlineAlways "a" "b"
  , a04: inlineAlways "a" "b" 1
  , a05: inlineAlways "a" "b" 1 2
  , a06: inlineAlways "a" "b" 1 2 3
  , a07: inlineAlways "a" "b" 1 2 3 4
  , a08: inlineAlways "a" "b" 1 2 3 4 5
  , a09: inlineAlways "a" "b" 1 2 3 4 5 6
  , a10: inlineAlways "a" "b" 1 2 3 4 5 6
  , a11: inlineAlways "a" "b" 1 2 3 4 5 6
  , a12: inlineAlways "a" "b" 1 2 3 4 5 6
  , a13: inlineAlways "a" "b" 1 2 3 4 5 6
  , a14: inlineAlways "a" "b" 1 2 3 4 5 6
  , a15: inlineAlways "a" "b" 1 2 3 4 5 6
  , a16: inlineAlways "a" "b" 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: inlineAlways "a" "b" 1 2 3 4 5 6
  }

test2_alwaysRec = test2_always.a02 "b" 1 2 3 4 5

test3_arity3 =
  { a01: inlineArity3
  , a02: inlineArity3 "a"
  , a03: inlineArity3 "a" "b"
  , a04: inlineArity3 "a" "b" 1
  , a05: inlineArity3 "a" "b" 1 2
  , a06: inlineArity3 "a" "b" 1 2 3
  , a07: inlineArity3 "a" "b" 1 2 3 4
  , a08: inlineArity3 "a" "b" 1 2 3 4 5
  , a09: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a10: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a11: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a12: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a13: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a14: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a15: inlineArity3 "a" "b" 1 2 3 4 5 6
  , a16: inlineArity3 "a" "b" 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: inlineArity3 "a" "b" 1 2 3 4 5 6
  }

test3_arity3Rec = test3_arity3.a02 "b" 1 2 3 4 5

-- This is just Functor with a bunch of additional args to illustrate directives.
class ClassName f where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> f a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> f b

data BoxNever a = BoxNever a

-- We use an instance name here to ensure we can reliably refer to the identifier in the directive
instance classNameBoxNever :: ClassName BoxNever where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> BoxNever a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> BoxNever b
  classMember f (BoxNever a) _1 _2 _3 _4 _5 _6 = BoxNever (f a)

test4_classMemberBoxNever =
  { a01: classMember
  , a02: classMember show
  , a03: classMember show (BoxNever 0)
  , a04: classMember show (BoxNever 0) 1
  , a05: classMember show (BoxNever 0) 1 2
  , a06: classMember show (BoxNever 0) 1 2 3
  , a07: classMember show (BoxNever 0) 1 2 3 4
  , a08: classMember show (BoxNever 0) 1 2 3 4 5
  , a09: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a10: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a11: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a12: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a13: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a14: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a15: classMember show (BoxNever 0) 1 2 3 4 5 6
  , a16: classMember show (BoxNever 0) 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: classMember show (BoxNever 0) 1 2 3 4 5 6
  }

data BoxAlways a = BoxAlways a

instance classNameBoxAlways :: ClassName BoxAlways where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> BoxAlways a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> BoxAlways b
  classMember f (BoxAlways a) _1 _2 _3 _4 _5 _6 = BoxAlways (f a)

test5_classMemberBoxAlways =
  { a01: classMember
  , a02: classMember show
  , a03: classMember show (BoxAlways 0)
  , a04: classMember show (BoxAlways 0) 1
  , a05: classMember show (BoxAlways 0) 1 2
  , a06: classMember show (BoxAlways 0) 1 2 3
  , a07: classMember show (BoxAlways 0) 1 2 3 4
  , a08: classMember show (BoxAlways 0) 1 2 3 4 5
  , a09: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a10: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a11: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a12: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a13: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a14: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a15: classMember show (BoxAlways 0) 1 2 3 4 5 6
  , a16: classMember show (BoxAlways 0) 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: classMember show (BoxAlways 0) 1 2 3 4 5 6
  }

data BoxArity3 a = BoxArity3 a

instance classNameBoxArity3 :: ClassName BoxArity3 where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> BoxArity3 a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> BoxArity3 b
  classMember f (BoxArity3 a) _1 _2 _3 _4 _5 _6 = BoxArity3 (f a)

test6_classMemberBoxArity3 =
  { a01: classMember
  , a02: classMember show
  , a03: classMember show (BoxArity3 0)
  , a04: classMember show (BoxArity3 0) 1
  , a05: classMember show (BoxArity3 0) 1 2
  , a06: classMember show (BoxArity3 0) 1 2 3
  , a07: classMember show (BoxArity3 0) 1 2 3 4
  , a08: classMember show (BoxArity3 0) 1 2 3 4 5
  , a09: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a10: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a11: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a12: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a13: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a14: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a15: classMember show (BoxArity3 0) 1 2 3 4 5 6
  , a16: classMember show (BoxArity3 0) 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: classMember show (BoxArity3 0) 1 2 3 4 5 6
  }

data SuperBoxNever f a = SuperBoxNever (f a)

-- Remember: constraints count as arguments to the dictionary
instance classNameSuperBoxNever :: (ClassName f) => ClassName (SuperBoxNever f) where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> SuperBoxNever f a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> SuperBoxNever f b
  classMember fn (SuperBoxNever fa) _1 _2 _3 _4 _5 _6 = SuperBoxNever (classMember fn fa _1 _2 _3 _4 _5 _6)

test7_classMemberSuperBoxNever =
  { a01: classMember
  , a02: classMember show
  , a03: classMember show (SuperBoxNever (BoxNever 0))
  , a04: classMember show (SuperBoxNever (BoxNever 0)) 1
  , a05: classMember show (SuperBoxNever (BoxNever 0)) 1 2
  , a06: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3
  , a07: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4
  , a08: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5
  , a09: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a10: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a11: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a12: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a13: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a14: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a15: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  , a16: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: classMember show (SuperBoxNever (BoxNever 0)) 1 2 3 4 5 6
  }

data SuperBoxAlways f a = SuperBoxAlways (f a)

instance classNameSuperBoxAlways :: (ClassName f) => ClassName (SuperBoxAlways f) where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> SuperBoxAlways f a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> SuperBoxAlways f b
  classMember fn (SuperBoxAlways fa) _1 _2 _3 _4 _5 _6 = SuperBoxAlways (classMember fn fa _1 _2 _3 _4 _5 _6)

test8_classMemberSuperBoxAlways =
  { a01: classMember
  , a02: classMember show
  , a03: classMember show (SuperBoxAlways (BoxAlways 0))
  , a04: classMember show (SuperBoxAlways (BoxAlways 0)) 1
  , a05: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2
  , a06: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3
  , a07: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4
  , a08: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5
  , a09: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a10: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a11: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a12: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a13: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a14: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a15: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  , a16: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: classMember show (SuperBoxAlways (BoxAlways 0)) 1 2 3 4 5 6
  }

data SuperBoxArity3 f a = SuperBoxArity3 (f a)

instance classNameSuperBoxArity3 :: (ClassName f) => ClassName (SuperBoxArity3 f) where
  classMember :: forall a b x1 x2 x3 x4 x5 x6. (a -> b) -> SuperBoxArity3 f a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> SuperBoxArity3 f b
  classMember fn (SuperBoxArity3 fa) _1 _2 _3 _4 _5 _6 = SuperBoxArity3 (classMember fn fa _1 _2 _3 _4 _5 _6)

test9_classMemberSuperBoxArity3 =
  { a01: classMember
  , a02: classMember show
  , a03: classMember show (SuperBoxArity3 (BoxArity3 0))
  , a04: classMember show (SuperBoxArity3 (BoxArity3 0)) 1
  , a05: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2
  , a06: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3
  , a07: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4
  , a08: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5
  , a09: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a10: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a11: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a12: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a13: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a14: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a15: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  , a16: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  -- `size > 16`, so only use directive (if any).
  , a17: classMember show (SuperBoxArity3 (BoxArity3 0)) 1 2 3 4 5 6
  }

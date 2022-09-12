module PureScript.Backend.Optimizer.Directives.Defaults where

defaultDirectives :: String
defaultDirectives =
  """
  -- arrays
  -- <TODO>

  -- assert
  -- <none>

  -- bifunctors
  Control.Biapply.biapplyFirst arity=1
  Control.Biapply.biapplySecond arity=1

  -- catenable-lists
  -- <none>

  -- console
  -- <none>

  -- const
  Data.Const.applicativeConst arity=1

  -- contravariant
  -- Data.Comparison.defaultComparison arity=1
  -- Data.Decidable.decidableOp arity=1
  -- Data.Decidable.lost arity=1
  -- Data.Decide.chooseOp arity=1
  -- Data.Decide.chosen arity=1
  -- Data.Divide.divideOp arity=1
  -- Data.Divide.divided arity=1
  -- Data.Divisible.divisibleOp arity=1
  -- Data.Equivalence.defaultEquivalence arity=1
  -- Data.Equivalence.comparisonEquivalence arity=1
  -- Data.Op.semigroupOp arity=1
  -- Data.Op.monoidOp arity=1
  -- Data.Op.contravariantOp arity=1

  -- control
  -- <TODO>

  -- datetime
  -- <TODO>

  -- distributive
  -- <TODO>

  -- effect
  -- <move inliners here>

  -- either
  -- <TODO>

  -- enums
  -- <TODO>

  -- exceptions
  -- <TODO>

  -- exists
  -- <TODO>

  -- filterable
  -- <TODO>

  -- foldable-traversable
  -- <TODO>

  -- foreign
  -- <TODO>

  -- foreign-object
  -- <TODO>

  -- free
  -- <TODO>

  -- functions
  -- <TODO>

  -- functors
  -- <TODO>

  -- gen
  -- <TODO>

  -- graphs
  -- <TODO>

  -- identity
  -- <TODO>

  -- integers
  -- <TODO>

  -- invariant
  -- <TODO>

  -- lazy
  -- <TODO>

  -- lcg
  -- <TODO>

  -- lists
  -- <TODO>

  -- maybe
  -- <TODO>

  -- metadata
  -- <none and this package can always be ignored>

  -- minibench
  -- <TODO>

  -- newtype
  -- <TODO>

  -- numbers
  -- <TODO>

  -- ordered-collections
  -- <TODO>

  -- orders
  -- <TODO>

  -- parallel
  -- <TODO>

  -- partial
  -- <TODO>

  -- prelude
  -- <TODO: move respective content here>

  -- profunctor
  -- <TODO>

  -- psci-support
  -- <TODO>

  -- quickcheck
  -- <TODO>

  -- random
  -- <TODO>

  -- record
  -- <TODO>

  -- refs
  -- <TODO>

  -- safe-coerce
  -- <TODO>

  -- semirings
  -- <TODO>

  -- st
  -- <TODO>

  -- strings
  -- <TODO>

  -- tailrec
  -- <TODO>

  -- transformers
  -- <TODO>

  -- tuples
  -- <TODO>

  -- type-equality
  -- <TODO>

  -- typelevel-prelude
  -- <TODO>

  -- unfoldable
  -- <TODO>

  -- unsafe-coerce
  -- <TODO>

  -- validation
  -- <TODO>

  -- Prelude

  Control.Applicative.liftA1 arity=1
  Control.Applicative.when arity=1
  Control.Applicative.unless arity=1
  Control.Applicative.applicativeFn.pure arity=1
  Control.Applicative.applicativeArray.pure arity=1

  Control.Apply.applyFirst arity=1
  Control.Apply.applySecond arity=1
  Control.Apply.lift2 arity=1
  Control.Apply.lift3 arity=1
  Control.Apply.lift4 arity=1
  Control.Apply.lift5 arity=1
  Control.Apply.applyFn.apply arity=2

  Control.Bind.bindFlipped arity=1
  Control.Bind.join arity=1
  Control.Bind.composeKleisli arity=1
  Control.Bind.composeKleisliFlipped arity=1
  Control.Bind.ifM arity=1
  Control.Bind.bindFn arity=2
  Control.Bind.discard arity=1

  Control.Category.categoryFn.identity always

  Control.Monad.ap arity=1
  Control.Monad.lift1 arity=1
  Control.Monad.whenM arity=1
  Control.Monad.unlessM arity=1

  Control.Semigroupoid.composeFlipped arity=1
  Control.Semigroupoid.semigroupoidFn.compose arity=2

  Data.Boolean.otherwise always

  Data.Bounded.boundedRecordCons arity=5
  Data.Bounded.boundedRecord arity=2

  Data.Bounded.Generic.genericBottom arity=1
  Data.Bounded.Generic.genericBottom' arity=1
  Data.Bounded.Generic.genericBottomNoArguments.genericBottom' always
  Data.Bounded.Generic.genericBottomArgument.genericBottom' arity=1
  Data.Bounded.Generic.genericBottomSum.genericBottom' arity=1
  Data.Bounded.Generic.genericBottomProduct.genericBottom' arity=2
  Data.Bounded.Generic.genericBottomConstructor.genericBottom' arity=1
  Data.Bounded.Generic.genericTop arity=1
  Data.Bounded.Generic.genericTop' arity=1
  Data.Bounded.Generic.genericTopNoArguments.genericTop' always
  Data.Bounded.Generic.genericTopArgument.genericTop' arity=1
  Data.Bounded.Generic.genericTopSum.genericTop' arity=1
  Data.Bounded.Generic.genericTopProduct.genericTop' arity=2
  Data.Bounded.Generic.genericTopConstructor.genericTop' arity=1

  Data.DivisionRing.leftDiv arity=1
  Data.DivisionRing.rightDiv arity=1
  Data.DivisionRing.divisionringNumber.recip arity=1

  Data.Eq.notEq arity=1
  Data.Eq.eqArray arity=1
  Data.Eq.eqRec arity=2
  Data.Eq.eqRowCons arity=4
  Data.Eq.notEq1 arity=1

  Data.Eq.Generic.genericEq arity=1
  Data.Eq.Generic.genericEq' arity=1
  Data.Eq.Generic.genericEqSum.genericEq' arity=2
  Data.Eq.Generic.genericEqProduct.genericEq' arity=2
  Data.Eq.Generic.genericEqConstructor.genericEq' arity=1
  Data.Eq.Generic.genericEqArgument.genericEq' arity=1

  Data.EuclideanRing.gcd arity=4
  Data.EuclideanRing.lcm arity=4

  Data.Generic.Rep.showSum.show arity=2
  Data.Generic.Rep.showProduct.show arity=2
  Data.Generic.Rep.showConstructor.show arity=2
  Data.Generic.Rep.showArgument.show arity=1
  Data.Generic.Rep.repOf arity=1

  Data.Function.flip arity=1
  Data.Function.const arity=1
  Data.Function.apply arity=2
  Data.Function.applyFlipped arity=2
  Data.Function.on arity=2

  Data.Functor.mapFlipped arity=1
  Data.Functor.void arity=1
  Data.Functor.voidRight arity=1
  Data.Functor.voidLeft arity=1
  Data.Functor.flap arity=1

  Data.HeytingAlgebra.heytingAlgebraBoolean.implies arity=2
  Data.HeytingAlgebra.heytingAlgebraFunction arity=1
  Data.HeytingAlgebra.heytingAlgebraRecord arity=2
  Data.HeytingAlgebra.heytingAlgebraRecordCons arity=4

  Data.HeytingAlgebra.Generic.genericHeytingAlgebraArgument arity=1
  Data.HeytingAlgebra.Generic.genericHeytingAlgebraProduct arity=2
  Data.HeytingAlgebra.Generic.genericHeytingAlgebraConstructor arity=1
  Data.HeytingAlgebra.Generic.genericFF arity=1
  Data.HeytingAlgebra.Generic.genericTT arity=1
  Data.HeytingAlgebra.Generic.genericImplies arity=1
  Data.HeytingAlgebra.Generic.genericConj arity=1
  Data.HeytingAlgebra.Generic.genericDisj arity=1
  Data.HeytingAlgebra.Generic.genericNot arity=1

  Data.Monoid.guard arity=2
  Data.Monoid.monoidRecordCons arity=4

  Data.Ord.comparing arity=2
  Data.Ord.ordRecordCons arity=4

  Data.Semigroup.semigroupFn.append arity=2
  Data.Semigroup.semigroupRecordCons arity=4

  Data.Show.showArray arity=1
  Data.Show.showRecord arity=3
  Data.Show.showRecordFieldsConsNil arity=2
  Data.Show.showRecordFieldsCons arity=3

  Control.Monad.ST.Internal.modify arity=2
  Effect.applyEffect.apply arity=2
  Effect.Ref.modify arity=2
  Record.Builder.build arity=1
  Record.Builder.rename arity=8
  """

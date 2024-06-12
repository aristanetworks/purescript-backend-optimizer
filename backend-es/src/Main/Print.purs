module Main.Print where


import Prelude

import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Milliseconds(..))
import PureScript.Backend.Optimizer.CoreFn (ModuleName)
import Data.Array (last, sort)
import Dodo as Dodo
import Dodo.Box as Dodo.Box
import Data.Number.Format (toString)


formatMs :: Milliseconds -> String
formatMs (Milliseconds m) = toString m <> "ms"

printTimings :: Array (Tuple ModuleName Milliseconds) -> String
printTimings =
  Dodo.print Dodo.plainText Dodo.twoSpaces
    <<< Dodo.indent
    <<< printTable
    <<< map (bimap (Dodo.text <<< append "* " <<< unwrap) (Dodo.text <<< formatMs))

printTotals :: Array (Tuple String Milliseconds) -> String
printTotals =
  Dodo.print Dodo.plainText Dodo.twoSpaces
    <<< printTable
    <<< map (bimap Dodo.text (Dodo.text <<< formatMs))

printTable :: forall a. Array (Tuple (Dodo.Doc a) (Dodo.Doc a)) -> Dodo.Doc a
printTable all = Dodo.Box.toDoc table
  where
  toBox = Dodo.print Dodo.Box.docBox Dodo.twoSpaces
  columns = bimap toBox toBox <$> all
  col1Width = fromMaybe 0 $ last $ sort $ (_.width <<< Dodo.Box.sizeOf <<< fst) <$> columns
  col2Width = fromMaybe 0 $ last $ sort $ (_.width <<< Dodo.Box.sizeOf <<< snd) <$> columns
  table = Dodo.Box.vertical $ printRow <$> columns
  printRow (Tuple a b) = Dodo.Box.horizontal
    [ Dodo.Box.resize { height: 1, width: col1Width } a
    , Dodo.Box.hpadding 3
    , Dodo.Box.resize { height: 1, width: col2Width } (Dodo.Box.halign Dodo.Box.End b)
    ]

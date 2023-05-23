module PureScript.Backend.Optimizer.Tracer where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Ident(..), ModuleName(..), Qualified(..))
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Lexer (lex)
import PureScript.CST.Parser.Monad (Parser, PositionedError, eof, runParser, take)
import PureScript.CST.Types (SourceToken)
import PureScript.CST.Types as CST

type TracedIdentifierFileResult =
  { errors :: Array (Tuple String PositionedError)
  , idents :: Set (Qualified Ident)
  }

parseTracedIdentifiersFile :: String -> TracedIdentifierFileResult
parseTracedIdentifiersFile = foldlWithIndex go { errors: [], idents: Set.empty } <<< String.split (Pattern "\n")
  where
  go line { errors, idents } str = case parseTracedIdentLine str of
    Left err ->
      { errors: Array.snoc errors (Tuple str (err { position { line = line } })), idents }
    Right Nothing ->
      { errors, idents }
    Right (Just ident) ->
      { errors, idents: Set.insert ident idents }

type TracedIdentifierHeaderResult =
  { errors :: Array (Tuple String PositionedError)
  , idents :: Set (Qualified Ident)
  }

parseTracedIdentifiersHeader :: ModuleName -> Array Comment -> TracedIdentifierHeaderResult
parseTracedIdentifiersHeader moduleName = foldl go { errors: [], idents: Set.empty }
  where
  go { errors, idents } = case _ of
    LineComment str
      | Just line <- String.stripPrefix (Pattern "@trace") $ String.trim str -> do
          let line' = String.trim line -- Trim again for leading space, makes errors better.
          case fst <$> runParser (lex line') parser of
            Left err ->
              { errors: Array.snoc errors (Tuple line' err), idents }
            Right ident ->
              { errors, idents: Set.insert (Qualified (Just moduleName) ident) idents }
    _ ->
      { errors, idents }

  parser = unqualified <* eof

parseTracedIdentLine :: String -> Either PositionedError (Maybe (Qualified Ident))
parseTracedIdentLine line = fst <$> runParser (lex line) parseTracedIdentMaybe

parseTracedIdentMaybe :: Parser (Maybe (Qualified Ident))
parseTracedIdentMaybe = Just <$> (qualified <* eof) <|> (Nothing <$ eof)

unqualified :: Parser Ident
unqualified = expectMap case _ of
  { value: CST.TokLowerName Nothing ident } ->
    Just $ Ident ident
  _ ->
    Nothing

qualified :: Parser (Qualified Ident)
qualified = expectMap case _ of
  { value: CST.TokLowerName (Just (CST.ModuleName mod)) ident } ->
    Just $ Qualified (Just (ModuleName mod)) (Ident ident)
  _ ->
    Nothing

expectMap :: forall a. (SourceToken -> Maybe a) -> Parser a
expectMap k = take \tok ->
  case k tok of
    Just a ->
      Right a
    Nothing ->
      Left $ UnexpectedToken tok.value

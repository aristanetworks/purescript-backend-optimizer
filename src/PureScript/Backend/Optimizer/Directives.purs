module PureScript.Backend.Optimizer.Directives
  ( parseDirectiveFile
  , parseDirectiveHeader
  , parseDirectiveLine
  , parseDirectiveExport
  , DirectiveFileResult
  , DirectiveHeaderResult
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Ident(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Interned (intern)
import PureScript.Backend.Optimizer.Semantics (EvalRef(..), InlineAccessor(..), InlineDirective(..), InlineDirectiveMap, insertDirective)
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Lexer (lex)
import PureScript.CST.Parser.Monad (Parser, PositionedError, eof, runParser, take)
import PureScript.CST.Types (IntValue(..), SourceToken, Token(..))
import PureScript.CST.Types as CST

type DirectiveFileResult =
  { errors :: Array (Tuple String PositionedError)
  , directives :: InlineDirectiveMap
  }

parseDirectiveFile :: String -> DirectiveFileResult
parseDirectiveFile = foldlWithIndex go { errors: [], directives: Map.empty } <<< String.split (Pattern "\n")
  where
  go line { errors, directives } str = case parseDirectiveLine str of
    Left err ->
      { errors: Array.snoc errors (Tuple str (err { position { line = line } })), directives }
    Right Nothing ->
      { errors, directives }
    Right (Just (Tuple key (Tuple acc val))) ->
      { errors, directives: insertDirective key acc val directives }

type DirectiveHeaderResult =
  { errors :: Array (Tuple String PositionedError)
  , locals :: InlineDirectiveMap
  , exports :: InlineDirectiveMap
  }

parseDirectiveHeader :: ModuleName -> Array Comment -> DirectiveHeaderResult
parseDirectiveHeader moduleName = foldl go { errors: [], locals: Map.empty, exports: Map.empty }
  where
  go { errors, locals, exports } = case _ of
    LineComment str
      | Just line <- String.stripPrefix (Pattern "@inline") $ String.trim str -> do
          let line' = String.trim line -- Trim again for leading space, makes errors better.
          case runParser (lex line') parser of
            Left err ->
              { errors: Array.snoc errors (Tuple line' err), locals, exports }
            Right (Tuple (Left (Tuple key (Tuple acc val))) _) ->
              { errors, locals, exports: insertDirective key acc val exports }
            Right (Tuple (Right (Tuple key (Tuple acc val))) _) ->
              { errors, locals: insertDirective key acc val locals, exports }
    _ ->
      { errors, locals, exports }

  parser =
    Left <$> parseDirectiveExport moduleName <|> Right <$> parseDirective

parseDirectiveLine :: String -> Either PositionedError (Maybe (Tuple EvalRef (Tuple InlineAccessor InlineDirective)))
parseDirectiveLine line = fst <$> runParser (lex line) parseDirectiveMaybe

parseDirectiveMaybe :: Parser (Maybe (Tuple EvalRef (Tuple InlineAccessor InlineDirective)))
parseDirectiveMaybe = Just <$> parseDirective <|> (Nothing <$ eof)

parseDirectiveExport :: ModuleName -> Parser (Tuple EvalRef (Tuple InlineAccessor InlineDirective))
parseDirectiveExport moduleName =
  ( ado
      keyword "export"
      ident <- unqualified
      accessor <- parseInlineAccessor
      directive <- parseInlineDirective
      in Tuple (EvalExtern (intern (Qualified (Just moduleName) ident))) (Tuple accessor directive)
  ) <* eof

parseDirective :: Parser (Tuple EvalRef (Tuple InlineAccessor InlineDirective))
parseDirective =
  ( ado
      qual <- qualified
      accessor <- parseInlineAccessor
      directive <- parseInlineDirective
      in Tuple (EvalExtern (intern qual)) (Tuple accessor directive)
  ) <* eof

parseInlineAccessor :: Parser InlineAccessor
parseInlineAccessor =
  InlineProp <$> (dot *> label)
    <|> InlineSpineProp <$> (dotDot *> dot *> label)
    <|> pure InlineRef

parseInlineDirective :: Parser InlineDirective
parseInlineDirective =
  InlineDefault <$ keyword "default"
    <|> InlineNever <$ keyword "never"
    <|> InlineAlways <$ keyword "always"
    <|> InlineArity <$> (keyword "arity" *> equals *> natural)

qualified :: Parser (Qualified Ident)
qualified = expectMap case _ of
  { value: CST.TokLowerName (Just (CST.ModuleName mod)) ident } ->
    Just $ Qualified (Just (ModuleName mod)) (Ident ident)
  _ ->
    Nothing

unqualified :: Parser Ident
unqualified = expectMap case _ of
  { value: CST.TokLowerName Nothing ident } ->
    Just $ Ident ident
  _ ->
    Nothing

label :: Parser String
label = expectMap case _ of
  { value: TokRawString lbl } ->
    Just lbl
  { value: TokString _ lbl } ->
    Just lbl
  { value: TokLowerName Nothing lbl } ->
    Just lbl
  _ ->
    Nothing

dot :: Parser Unit
dot = expectMap case _ of
  { value: TokDot } ->
    Just unit
  _ ->
    Nothing

dotDot :: Parser Unit
dotDot = expectMap case _ of
  { value: TokSymbolName Nothing ".." } ->
    Just unit
  _ ->
    Nothing

equals :: Parser Unit
equals = expectMap case _ of
  { value: TokEquals } ->
    Just unit
  _ ->
    Nothing

keyword :: String -> Parser Unit
keyword word1 = expectMap case _ of
  { value: TokLowerName Nothing word2 } | word1 == word2 ->
    Just unit
  _ ->
    Nothing

natural :: Parser Int
natural = expectMap case _ of
  { value: TokInt _ (SmallInt n) } | n > 0 ->
    Just n
  _ ->
    Nothing

expectMap :: forall a. (SourceToken -> Maybe a) -> Parser a
expectMap k = take \tok ->
  case k tok of
    Just a ->
      Right a
    Nothing ->
      Left $ UnexpectedToken tok.value

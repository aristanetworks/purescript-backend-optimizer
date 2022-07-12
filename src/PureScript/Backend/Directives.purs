module PureScript.Backend.Directives where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Semantics (EvalRef(..), InlineDirective(..))
import PureScript.Backend.Syntax (BackendAccessor(..))
import PureScript.CST.Errors (ParseError)
import PureScript.CST.Lexer as Lexer
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (IntValue(..), Token(..))
import PureScript.CST.Types as CST
import PureScript.CoreFn (Ident(..), ModuleName(..), Qualified(..))

parseDirective :: Array String -> Map EvalRef InlineDirective
parseDirective = Map.fromFoldable <<< Array.mapMaybe parseLine

parseLine :: String -> Maybe (Tuple EvalRef InlineDirective)
parseLine = Lexer.lex >>> tokenStreamToArray >>> case _ of
  Right [ TokLowerName modName fn, directive ] ->
    inlineIdentifier modName fn Nothing directive
  Right [ TokLowerName modName fn, TokDot, TokLowerName Nothing fieldName, directive ] ->
    inlineIdentifier modName fn (Just (GetProp fieldName)) directive
  _ -> Nothing
  where
  inlineIdentifier
    :: Maybe CST.ModuleName
    -> String
    -> Maybe BackendAccessor
    -> Token
    -> Maybe (Tuple EvalRef InlineDirective)
  inlineIdentifier cstModName identifier externAccessor directive = do
    let
      qi = Qualified (map cstToBackendMod cstModName) (Ident identifier)
    Tuple (EvalExtern qi externAccessor) <$> inlineDirective directive

  inlineDirective :: Token -> Maybe InlineDirective
  inlineDirective = case _ of
    -- An arity of `0` or some negative number wouldn't make sense
    TokInt _ (SmallInt arity) | arity > 0 -> Just $ InlineArity arity
    TokLowerName Nothing "always" -> Just InlineAlways
    TokLowerName Nothing "never" -> Just InlineNever
    _ -> Nothing

  cstToBackendMod (CST.ModuleName mn) = ModuleName mn

  tokenStreamToArray :: TokenStream -> Either ParseError (Array Token)
  tokenStreamToArray = go []
    where
    go acc = TokenStream.step >>> case _ of
      TokenEOF _ _ ->
        Right acc
      TokenError _ err _ _ ->
        Left err
      TokenCons tok _ next _ ->
        go (Array.snoc acc tok.value) next

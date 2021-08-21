{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor (second)
import Data.Char (isAlpha, isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import System.Environment
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import Text.Regex.TDFA

newtype CommentMarker = LineComment T.Text
  deriving (Eq, Ord, Show)

type VarName = T.Text

newtype VarDef = RegexVar Regex

instance Show VarDef where
  show (RegexVar _) = "RegexVar #<regex>"

type Directive = T.Text

lineCommentHeader :: T.Text
lineCommentHeader = "AUTOCOMF COMMENT LINE "

findCommentMarker :: T.Text -> CommentMarker
findCommentMarker s
  | (_, s') <- T.breakOn lineCommentHeader s,
    c <- T.takeWhile (not . isSpace) $ T.drop (T.length lineCommentHeader) s' =
    LineComment c
  | otherwise =
    LineComment "#"

findDirective :: CommentMarker -> T.Text -> Maybe (Directive, T.Text)
findDirective (LineComment c) s = do
  let (bef, s') =
        second (T.stripStart . T.drop (T.length c)) $ T.breakOn c s
  guard $ not $ T.null s'
  if "AUTOCOMF" `T.isPrefixOf` s'
    then pure $ T.breakOn "\n" s'
    else findDirective (LineComment c) s'

findDirectives :: CommentMarker -> T.Text -> [Directive]
findDirectives c s =
  case findDirective c s of
    Nothing -> []
    Just (dir, s') -> dir : findDirectives c s'

type Parser = Parsec Void T.Text

token :: T.Text -> Parser ()
token s = void $ chunk s <* space

pVarName :: Parser VarName
pVarName = takeWhileP (Just "letter") isAlpha <* space

pVarDef :: Parser VarDef
pVarDef = do
  token "REGEX"
  r <- makeRegexOptsM blankCompOpt defaultExecOpt . T.unpack =<< takeRest
  pure $ RegexVar r

pVarDir :: Parser (VarName, VarDef)
pVarDir = do
  token "AUTOCOMF"
  token "VAR"
  (,) <$> pVarName <*> pVarDef

findVars :: [Directive] -> M.Map VarName VarDef
findVars = foldMap varFromDir
  where
    varFromDir dir =
      maybe mempty (uncurry M.singleton) $ parseMaybe pVarDir dir

main :: IO ()
main = do
  input <- T.getContents
  let c = findCommentMarker input
      dirs = findDirectives c input
      vars = findVars dirs
  print dirs
  print vars

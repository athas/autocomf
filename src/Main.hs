{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor (second)
import Data.Char (isAlpha, isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import qualified System.Console.Readline as Readline
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec hiding (match, token)
import Text.Megaparsec.Char
import Text.Regex.TDFA

newtype CommentMarker = LineComment T.Text
  deriving (Eq, Ord, Show)

type VarName = T.Text

data VarDef = RegexVar T.Text Regex

instance Show VarDef where
  show (RegexVar r _) = "RegexVar " <> T.unpack r

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
  let (_bef, s') =
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
  r <- takeRest
  r' <- makeRegexOptsM blankCompOpt defaultExecOpt $ T.unpack r
  pure $ RegexVar r r'

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

getVarValFromUser :: VarName -> VarDef -> IO T.Text
getVarValFromUser v (RegexVar r regex) = do
  T.putStrLn $ "Enter value for " <> v <> " (" <> r <> "):"
  l' <- fmap T.pack <$> Readline.readline "> "
  case fmap (\l -> (l, match regex l)) l' of
    Just (l, True) -> pure l
    _ -> do
      T.putStrLn "No.  I already told you what was expected.  Try again."
      getVarValFromUser v $ RegexVar r regex

getVarValsFromUser :: M.Map VarName VarDef -> IO (M.Map VarName T.Text)
getVarValsFromUser = fmap M.fromList . mapM f . M.toList
  where
    f (v, def) =
      (v,) <$> getVarValFromUser v def

pUseDir :: Parser T.Text
pUseDir = do
  token "#"
  token "AUTOCOMF"
  token "USE"
  takeRest

instantiateUseDir :: M.Map VarName T.Text -> T.Text -> T.Text
instantiateUseDir vals = foldMap f . T.splitOn "¤"
  where
    f x = fromMaybe x $ M.lookup x vals

indentAs :: T.Text -> T.Text -> T.Text
indentAs x y = T.takeWhile isSpace x <> y

substituteVars :: M.Map VarName T.Text -> T.Text -> T.Text
substituteVars vals = T.unlines . recurse . T.lines
  where
    recurse [] = []
    recurse (l1 : l2 : ls)
      | Just usedir <- parseMaybe pUseDir l1 =
        l1 : indentAs l2 (instantiateUseDir vals usedir) : ls
    recurse (l : ls) =
      l : recurse ls

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
    [f] -> pure f
    _ -> do
      T.hPutStrLn stderr "Usage: autocomf <file>"
      exitFailure
  input <- T.readFile f
  let c = findCommentMarker input
      dirs = findDirectives c input
      vars = findVars dirs
  vals <- getVarValsFromUser vars
  T.writeFile f $ substituteVars vals input

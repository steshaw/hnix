module Main where

import Nix.Parser
import Nix.Pretty
import Nix.Eval
import Nix.Types

import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import System.Environment
import System.IO

import qualified Data.Map as Map

nix :: FilePath -> IO ()
nix path = do
  res <- parseNixFile path
  case res of
    Failure e -> hPutStrLn stderr $ "Parse failed: " ++ show e
    Success n -> do
      displayIO stdout $ renderPretty 0.4 80 (prettyNix n <> hardline)
      top <- prettyThunk (evalExpr n Map.empty)
      displayIO stdout . renderPretty 0.4 80 $ top <> hardline

main :: IO ()
main = do
    [path] <- getArgs
    nix path

-- | Parser test script
-- runghc -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/  StockholmAlignmentTest.hs input_file
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import System.Directory
import System.Process
import Control.Monad    
import Data.Either
import Data.Either.Unwrap
import Bio.StockholmParser
    
main = do
  args <- getArgs
  let input_file = (head args)
  parsedinput <-  readStockholm input_file
  let rightInput = fromRight parsedinput
  print rightInput


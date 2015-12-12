import           Control.Monad
import           System.Environment (getArgs)

import           BF

main = do
  (file : _) <- getArgs
  contents <- readFile file
  run $ filter (`elem` "<>,.-+[]") contents

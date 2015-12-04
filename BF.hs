module BF (execute) where

import           Control.Monad.State
import           Data.Char           (chr, ord)
import           Data.Maybe          (fromJust)

import qualified Data.Map            as M

data ArrayState = ArrayState
    { ip     :: Int             -- ^ instruction pointer
    , ptr    :: Int             -- ^ heap pointer
    , memory :: M.Map Int Int   -- ^ heap
    } deriving Show

type AS = StateT ArrayState IO

maxN      = 100 :: Int
enumerate = zip [0..] :: [a] -> [(Int, a)]
array     = M.fromList . enumerate $ replicate maxN 0 :: M.Map Int Int

command :: Char -> M.Map Int Int -> AS ()
command c jt = case c of
    '>' -> modify $ \s -> s { ptr = (ptr s) + 1, ip = (ip s) + 1 }
    '<' -> modify $ \s -> s { ptr = (ptr s) - 1, ip = (ip s) + 1 }
    '+' -> modify $ \s -> s { memory = M.adjust (+1) (ptr s) (memory s)
                            , ip = (ip s) + 1 }
    '-' -> modify $ \s -> s { memory = M.adjust pred (ptr s) (memory s)
                            , ip = (ip s) + 1 }
    ',' -> do x <- lift getChar
              modify $ \s -> s { memory = M.insert (ptr s) (ord x) (memory s)
                               , ip = (ip s) + 1 }
    '.' -> do s <- get
              let x = chr . fromJust $ M.lookup (ptr s) (memory s)
              modify $ \s -> s { ip = (ip s) + 1 }
              lift $ putChar x
    '[' -> do s <- get
              if (M.lookup (ptr s) (memory s)) /= Just 0
              then modify $ \s -> s { ip = (ip s) + 1 }
              else modify $ \s -> s { ip = fromJust $ M.lookup (ip s) jt }
    ']' -> do s <- get
              if (M.lookup (ptr s) (memory s)) == Just 0
              then modify $ \s -> s { ip = (ip s) + 1 }
              else modify $ \s -> s { ip = fromJust $ M.lookup (ip s) jt }
    _   -> return ()

jumpTable :: String -> [(Int,Int)]
jumpTable code = go [] (enumerate code)
    where go _ [] = []
          go stack (x:xs) = let (i,c) = x in
                            case c of
                              '[' -> go (i:stack) (xs)
                              ']' -> (top, i+1) : (i, top+1) : go (tail stack) xs
                              _   -> go stack xs
              where top = head stack

execute :: String -> AS ()
execute code = do
  let jt  = M.fromList $ jumpTable code
      len = length code
  s <- get
  if len == ip s
  then return ()
  else do command (code !! ip s) jt
          execute code

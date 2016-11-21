{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BF where

import           Control.Monad.State
import           Data.Char           (chr, ord)
import           Data.Maybe          (fromJust)

import qualified Data.Map            as M

data ArrayState = ArrayState
    { ip     :: Int             -- ^ instruction pointer
    , ptr    :: Int             -- ^ data pointer
    , memory :: M.Map Int Int   -- ^ tape
    } deriving Show

newtype Interpreter a = Interpreter (StateT ArrayState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ArrayState)

maxN      = 100 :: Int
enumerate = zip [0..] :: [a] -> [(Int, a)]
array     = M.fromList . enumerate $ replicate maxN 0 :: M.Map Int Int

runInterpreter :: Interpreter () -> IO ()
runInterpreter (Interpreter i) = evalStateT i (ArrayState 0 0 array)

debugInterpreter (Interpreter i) = execStateT i (ArrayState 0 0 array)

interpreter :: String -> Interpreter ()
interpreter code = execute
    where execute = do st <- get
                       let cmd = code !! ip st
                       unless (len == ip st) $
                              eval cmd table >> execute
          table = M.fromList $ jumpTable code
          len = length code

incrIP = modify $ \s -> s { ip = ip s + 1 }

eval :: Char -> M.Map Int Int -> Interpreter ()
eval '>' jt = incrIP >> modify (\s -> s { ptr = ptr s + 1 })
eval '<' jt = incrIP >> modify (\s -> s { ptr = ptr s - 1 })
eval '+' jt = incrIP >>
              modify (\s -> s { memory = M.adjust succ (ptr s) (memory s) })
eval '-' jt = incrIP >>
              modify (\s -> s { memory = M.adjust pred (ptr s) (memory s) })
eval ',' jt = do x <- liftIO getChar
                 incrIP
                 modify (\s -> s { memory = M.insert (ptr s) (ord x) (memory s) })
eval '.' jt = do st <- get
                 let x = chr . fromJust $ M.lookup (ptr st) (memory st)
                 incrIP
                 liftIO $ putChar x
eval '[' jt = do st <- get
                 if M.lookup (ptr st) (memory st) /= Just 0
                 then incrIP
                 else modify (\s -> s { ip = fromJust $ M.lookup (ip s) jt })
eval ']' jt = do st <- get
                 if M.lookup (ptr st) (memory st) == Just 0
                 then incrIP
                 else modify (\s -> s { ip = fromJust $ M.lookup (ip s) jt })
eval _ _ = return ()

jumpTable :: String -> [(Int,Int)]
jumpTable code = go [] (enumerate code)
    where go _ [] = []
          go stack (x:xs) = let (i,c) = x in
                            case c of
                              '[' -> go (i:stack) xs
                              ']' -> (top, i+1) : (i, top+1) : go (tail stack) xs
                              _   -> go stack xs
              where top = head stack

main = do
  code <- getContents
  runInterpreter (interpreter code)

{-# LANGUAGE TemplateHaskell #-}

module BF (run) where

import           Control.Monad.State
import           Data.Char           (chr, ord)
import           Data.Maybe          (fromJust)
import           Lens.Micro.Platform

import qualified Data.Map            as M
import qualified Data.Vector         as V

data ArrayState = ArrayState
    { _ip     :: !Int           -- ^ instruction pointer
    , _ptr    :: !Int           -- ^ heap pointer
    , _memory :: M.Map Int Int  -- ^ heap
    } deriving Show
makeLenses ''ArrayState

type AS = StateT ArrayState IO

maxN         = 350 :: Int
enumerate    = zip [0..] :: [a] -> [(Int, a)]
array        = M.fromList . enumerate $ replicate maxN 0 :: M.Map Int Int

jumpTable :: String -> [(Int,Int)]
jumpTable code = go [] (enumerate code)
    where go _ [] = []
          go stack (x:xs) = let (i,c) = x in
                            case c of
                              '[' -> go (i:stack) xs
                              ']' -> (top, i+1) : (i, top+1) : go (tail stack) xs
                              _   -> go stack xs
              where top = head stack

command :: V.Vector Char -> M.Map Int Int -> AS ()
command code jt = do
  st <- get
  let c = code V.! (st^.ip)
  ip += 1
  case c of
    '>' -> ptr += 1
    '<' -> ptr -= 1
    '+' -> memory .= (ix (st^.ptr) %~ (+1)       $ st^.memory)
    '-' -> memory .= (ix (st^.ptr) %~ subtract 1 $ st^.memory)
    ',' -> do x <- lift getChar
              memory .= (at (st^.ptr) .~ Just (ord x) $ st^.memory)
    '.' -> lift . putChar . chr . fromJust $ st^.memory^.at (st^.ptr)
    '[' -> when (st^.memory^.at (st^.ptr) == Just 0)
             $ ip .= fromJust (jt^.at (st^.ip))
    ']' -> when (st^.memory^.at (st^.ptr) /= Just 0)
             $ ip .= fromJust (jt^.at (st^.ip))
    _   -> return ()

run :: String -> IO ()
run code = evalStateT (execute code') (ArrayState 0 0 array)
    where jt    = M.fromList $ jumpTable code
          code' = V.fromList code
          len   = V.length code'
          execute cs = do
            s <- get
            when (len /= (s^.ip)) $ command cs jt >> execute cs

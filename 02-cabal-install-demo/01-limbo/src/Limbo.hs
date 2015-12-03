module Limbo (readFile') where

import System.IO
import Control.DeepSeq

readFile' :: IO String
readFile' = do
    h <- openFile "f" ReadMode
    s <- hGetContents h
    s `deepseq` hClose h
    return s

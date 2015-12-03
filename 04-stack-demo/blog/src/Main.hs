{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad      (unless)
import           Haskonf            (appDir, binName, buildDo, defaultFlagsVerbose, copyReal,
                                     doesConfigExist, runFrom)
import           Paths_blog
import           System.Environment (getArgs, getProgName)

pname :: String
pname = "blog"

main :: IO ()
main = do
  real <- getDataFileName $ pname ++ ".hs"
  doesConfigExist pname >>= (flip unless) (copyReal pname real)
  args <- getArgs
  _ <- buildDo pname (Just $ defaultFlagsVerbose pname) False
  launch args

launch :: [String] -> IO ()
launch args = do
  dir  <- appDir  pname
  me   <- getProgName
  runFrom me dir (binName pname) args

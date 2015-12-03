{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Map           (fromList)
import           Data.Text          (pack)
import           Data.Time.Format
import           System.Environment (getArgs)

import           Blog

cc :: TextText
cc = fromList [ ("Cmd",    "ssh -p 21984")
              , ("Remote", "memorici.de")
              , ("Path",   "github/lue") ]

bb :: TextTexts
bb = fromList [ ("BlogCat", ["draft", "ru", "life", "tech"]) ]

ff :: PathGenerator t
ff "tech" = defaultPathFn "universe"
ff x      = defaultPathFn x

cfg :: BlogCfg
cfg = BlogCfg { connection = cc, blog_spec = bb, pathFn = ff }

main :: IO ()
main = do
  args  <- getArgs
  let [file, cat] = map pack args
  ok <- post cfg file cat
  putStrLn $ show ok

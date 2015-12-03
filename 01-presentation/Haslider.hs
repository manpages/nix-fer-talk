module Haslider where

present :: String -> IO String
present _ = return $ unlines [ "slide <- mZero"
                             , ":!clear"
                             , ":next" ]

next :: String -> IO String
next _ = do
  ioSlide <- readFile ".nextSlide"
  return $ unlines [ "slide <- mPlusPlus slide"
                   , "lastRead <- readFile $ slide ++ \".txt\""
                   , "putStr lastRead"
                   , ":!read"
                   , ":!clear"
                   , "lastRead <- readFile $ slide ++ \".hs\""
                   , "putStr lastRead"
                   , ":load " ++ ioSlide ++ ".hs"
                   ]

-- | Some trolling, yes.
mZero :: IO String
mZero = do
  writeFile ".nextSlide" "1"
  return "0"

mPlusPlus :: String -> IO String
mPlusPlus x = do
  writeFile ".nextSlide" x1
  return $ x1
  where
    x1 = show $ (read x) + 1

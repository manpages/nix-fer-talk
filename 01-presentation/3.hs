-- λ>                                         | 3/..|                                        <λ






main :: IO ()
main = do
  putStrLn "Please enter a Number"
  x <- getLine
  putStr . show . (*) 2 $ read x





-- λ>                                                                                        <λ

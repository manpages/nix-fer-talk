module OldStyleMain where

import System.IO.Unsafe (unsafePerformIO)

data Request =
    ReadChan String
  | AppendChan String String

data Response =
    Success
  | Str String
  deriving Show

-- Execute a request using the IO monad and return the corresponding Response.
executeRequest :: Request -> IO Response
executeRequest (AppendChan "stdout" message) = do
  putStr message
  return Success
executeRequest (AppendChan chan _) =
  error ("Output channel " ++ chan ++ " not supported")
executeRequest (ReadChan "stdin") = do
  input <- getLine
  return $ Str input
executeRequest (ReadChan chan) =
  error ("Input channel " ++ chan ++ " not supported")

-- Take an old style main function and turn it into an IO action
runMain :: ([Response] -> [Request]) -> IO ()
runMain oldStyleMain = do
  let responses = map (unsafePerformIO . executeRequest) . oldStyleMain $ responses
  foldr seq (return ()) responses

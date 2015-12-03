-- λ>                                         | 2/..|                                        <λ


import OldStyleMain (Request (..), Response (..), runMain)

main' :: [Response] -> [Request]
main' responses =
  [
    AppendChan "stdout" "Please enter a Number\n",
    ReadChan "stdin",
    AppendChan "stdout" . show $ enteredNumber * 2
  ]
  where (Str input) = responses !! 1
        firstLine = head . lines $ input
        enteredNumber = read firstLine


-- λ>                                                                                        <λ

module Homework1.Task1 where

stringSum :: String -> Int
stringSum = sum . map (read . cutPlus) . words
  where cutPlus ('+':s) = s
        cutPlus other = other

tests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
        , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
        , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
        ]

mustFail = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

advancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]
advancedMustFail = ["1+1", "++1", "-+1", "+-1", "1 + 1"]

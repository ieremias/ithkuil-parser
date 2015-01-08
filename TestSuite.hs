module Test where
import Ithkuil

data Test = Test String Bool

passed :: Test -> Bool
passed (Test _ x) = x 
result :: Test -> String
result (Test r _) = r

testAll ts = foldl test1 ([], 0, 0) ts where
    test1 (results, pass, fail) t
      | passed t = ((result t) : results, pass + 1, fail)
      | otherwise = ((result t) : results, pass, fail + 1) 
   
main = do mapM_ putStrLn results
          putStrLn ("Passed: " ++ (show pass))
          putStrLn ("Failed: " ++ (show fail))
        where 
            (results, pass, fail) = testAll tests
            tests = testParse `map` 
                [
                  "phal"
                , "phe'l"
                , "eqoec"
                , "uaklarsla"
                , "uiphawatluxe'n"
                , "hremsoqaitsurkoi"
                , "qhul-lyai'sviksei'arpipto'ks"
                ]
                
testParse x
  | Right result <- pw x = Test ("PASSED:" ++ x ++ show result) True
  | Left  result <- pw x = Test ("FAILED:" ++ x ++ show result) False


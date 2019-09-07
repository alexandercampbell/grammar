import           Control.Monad

import           Grammar

verbose = True

assert :: String -> Bool -> IO ()
assert msg cond = if cond
  then when verbose $ putStrLn $ "Success: " ++ msg
  else putStrLn $ "FAIL: " ++ msg

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg expected actual = if expected == actual
  then
    when verbose
    $  putStrLn
    $  "Success: expected "
    ++ show expected
    ++ ", got it"
  else
    putStrLn $ "FAIL: expected " ++ show expected ++ ", actual " ++ show actual


main :: IO ()
main = assertEq "Test" True True

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
        putStrLn
        $  "FAIL: expected "
        ++ show expected
        ++ ", actual "
        ++ show actual

arithmeticGrammar =
    let number = OneOrMore $ Disjunction $ map (Literal . show) [0 .. 9]
        plus   = Sequence [number, Literal "+", expr]
        minus  = Sequence [number, Literal "-", expr]
        expr   = Disjunction [plus, minus, number]
    in  Grammar { start = expr }

main :: IO ()
main = do
    assert "literal rule matches literal text" $ matchRule (Literal "1") "1"
    assert "disjunction matches one of the options" $ matchRule
        (Disjunction [Literal "1", Literal "2", Literal "3"])
        "2"
    assert "arithmeticGrammar matches basic string"
        $ matches arithmeticGrammar "1 + 2 - 3"

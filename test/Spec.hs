import           Control.Monad

import           Grammar

verbose = True

assert :: String -> Bool -> IO ()
assert msg cond = if cond
    then when verbose $ putStrLn $ "Success: " ++ msg
    else putStrLn $ "FAIL: " ++ msg

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg expected actual = if expected == actual
    then when verbose $ putStrLn $ "Success: " ++ msg
    else
        putStrLn
        $  "FAIL: "
        ++ msg
        ++ ". Expected "
        ++ show expected
        ++ ", actual "
        ++ show actual

simpleArithmeticGrammar =
    let number = OneOrMore $ Disjunction $ map (Literal . show) [0 .. 9]
        plus   = Sequence [number, Literal "+", expr]
        minus  = Sequence [number, Literal "-", expr]
        expr   = Disjunction [plus, minus, number]
    in  Grammar { start = expr }

advancedArithmeticGrammar =
    let whitespace = ZeroOrMore $ Literal " "
        number     = Sequence
            [ OneOrMore $ Disjunction $ map (Literal . show) [0 .. 9]
            -- optional scientific notation
            , Optional $ Sequence
                [ Disjunction [Literal "e", Literal "E"]
                , Optional $ Literal "-"
                , OneOrMore $ Disjunction $ map (Literal . show) [0 .. 9]
                ]
            ]
        term = Disjunction
            [ Sequence [whitespace, number, whitespace]
            , Sequence [whitespace, Literal "(", expr, Literal ")", whitespace]
            ]
        expr = Disjunction
            [ Sequence [term, Literal "+", expr]
            , Sequence [term, Literal "-", expr]
            , Sequence [term, Literal "*", expr]
            , Sequence [term, Literal "/", expr]
            , Sequence [term, Literal "^", expr]
            , term
            ]
    in  Grammar { start = expr }

main :: IO ()
main = do
    assertEq "literal rule matches literal text" (Just "")
        $ matchRule (Literal "1") "1"
    assertEq "disjunction matches one of the options" (Just ".")
        $ matchRule (Disjunction [Literal "1", Literal "2", Literal "3"]) "2."
    assertEq "sequence matches expected sequence" (Just ".")
        $ matchRule (Sequence [Literal "1", Literal "2", Literal "3"]) "123."
    assertEq "sequence doesn't match out-of-order sequence" (Nothing)
        $ matchRule (Sequence [Literal "1", Literal "2", Literal "3"]) "132."
    assertEq "sequence doesn't match too-short sequence" (Nothing)
        $ matchRule (Sequence [Literal "1", Literal "2", Literal "3"]) "12"
    assertEq "ZeroOrMore matches nothing" (Just "")
        $ matchRule (ZeroOrMore $ Literal "1") ""
    assertEq "ZeroOrMore matches nothing and returns remainder" (Just "23")
        $ matchRule (ZeroOrMore $ Literal "1") "23"
    assertEq "OneOrMore requires at least one" (Nothing)
        $ matchRule (OneOrMore $ Literal "1") "23"
    assertEq "OneOrMore is greedy" (Just "23")
        $ matchRule (OneOrMore $ Literal "1") "1111123"
    assert "simpleArithmeticGrammar matches 4"
        $ matches simpleArithmeticGrammar "4"
    assert "simpleArithmeticGrammar matches 1+2-32"
        $ matches simpleArithmeticGrammar "1+2-32"
    assert "simpleArithmeticGrammar doesn't match text" $ not $ matches
        simpleArithmeticGrammar
        "text"
    assert "advancedArithmeticGrammar matches 1e9"
        $ matches advancedArithmeticGrammar "1e9"
    assert "advancedArithmeticGrammar matches 15e-7"
        $ matches advancedArithmeticGrammar "15e-7"
    assert "advancedArithmeticGrammar matches 1 + 2"
        $ matches advancedArithmeticGrammar "1 + 2"
    assert "advancedArithmeticGrammar matches (1 + 2)"
        $ matches advancedArithmeticGrammar "(1 + 2)"
    assert "advancedArithmeticGrammar matches (3e5 - 17)"
        $ matches advancedArithmeticGrammar "(3e5 - 17)"
    assert "advancedArithmeticGrammar matches 15 * (3e5 - 17)"
        $ matches advancedArithmeticGrammar "15 * (3e5 - 17)"
    assert "advancedArithmeticGrammar matches (3e5 - 17) * 15"
        $ matches advancedArithmeticGrammar "(3e5 - 17) * 15"
    assert "advancedArithmeticGrammar matches 1 + 2 * (3e5 - 17)"
        $ matches advancedArithmeticGrammar "1 + 2 * (3e5 - 17)"
    assert "advancedArithmeticGrammar fails to match 1 + + 2 * (3e5 - 17)"
        $ not
        $ matches advancedArithmeticGrammar "1 + + 2 * (3e5 - 17)"
    assert "advancedArithmeticGrammar fails to match 1e7e3" $ not $ matches
        advancedArithmeticGrammar
        "1e7e3"

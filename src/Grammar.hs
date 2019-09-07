module Grammar where

data Terminal = String

data Rule
    = RuleTerminal Terminal
    | RuleDisjunction [Rule]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

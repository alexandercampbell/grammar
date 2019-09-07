module Grammar where

data Rule
    = Literal String
    | OneOrMore Rule
    | Disjunction [Rule]
    | Sequence [Rule]
    deriving (Eq, Show)

data Grammar = Grammar
    { start :: Rule
    }
    deriving (Eq, Show)

matchRule :: Rule -> String -> Bool
matchRule (Literal     l   ) s = l == s
matchRule (Disjunction list) s = any id $ map (\r -> matchRule r s) list

matches :: Grammar -> String -> Bool
matches grammar str = False

someFunc :: IO ()
someFunc = putStrLn "someFunc"

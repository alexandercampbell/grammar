module Grammar where

import           Data.List

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

-- Return the remainder of the string in the case that a match is made;
-- otherwise, return Nothing
matchRule :: Rule -> String -> Maybe String
matchRule (Literal l) s =
    if isPrefixOf l s then Just $ drop (length l) s else Nothing
matchRule (Disjunction possibles) s =
    head $ filter (/= Nothing) $ map (\r -> matchRule r s) possibles
matchRule (Sequence []              ) s = Just s
matchRule (Sequence (first : others)) s = case matchRule first s of
    Just remainder -> matchRule (Sequence others) remainder
    Nothing        -> Nothing

matches :: Grammar -> String -> Bool
matches grammar str = False

someFunc :: IO ()
someFunc = putStrLn "someFunc"

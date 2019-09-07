module Grammar where

import           Data.Maybe
import           Data.List

data Rule
    = Literal String
    | ZeroOrMore Rule
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
matchRule (ZeroOrMore r) s = case matchRule r s of
    Just remainder -> matchRule (ZeroOrMore r) remainder
    Nothing        -> Just s
matchRule (OneOrMore r) s = case matchRule r s of
    Just remainder -> matchRule (ZeroOrMore r) remainder
    Nothing        -> Nothing
matchRule (Disjunction possibles) s =
    case filter (/= Nothing) $ map (\r -> matchRule r s) possibles of
        []     -> Nothing
        x : xs -> x
matchRule (Sequence []              ) s = Just s
matchRule (Sequence (first : others)) s = case matchRule first s of
    Just remainder -> matchRule (Sequence others) remainder
    Nothing        -> Nothing

matches :: Grammar -> String -> Bool
matches grammar str = matchRule (start grammar) str == Just ""

module Autocomplete.Core (WasAutoCompleteMatchFound (..), findLongestCommonPrefix, findBuiltInAutoCompleteMatch, findBuiltInAutoCompleteMatch') where

import Data.List (isInfixOf, isPrefixOf, maximumBy)
import Data.Ord (comparing)

data WasAutoCompleteMatchFound = NoAutoCompleteMatchFound | AutoCompleteMatchFound String | AutoCompleteMatchesFound [String] deriving (Show)

findLongestCommonPrefix :: [String] -> Maybe String
findLongestCommonPrefix options =
  let prefixWithMostMembers = getPrefixWithMostMembers $ map (\option -> (option, findCommonPrefixes option options)) options
   in if prefixOccursMoreThanOnce prefixWithMostMembers options then Just prefixWithMostMembers else Nothing

prefixOccursMoreThanOnce :: String -> [String] -> Bool
prefixOccursMoreThanOnce prefix options = length (findCommonPrefixes prefix options) > 0

getPrefixWithMostMembers :: [(String, [String])] -> String
getPrefixWithMostMembers options = fst $ maximumBy (comparing (length . snd)) options

findCommonPrefixes :: String -> [String] -> [String]
findCommonPrefixes prefix = filter (\option -> prefix /= option && prefix `isPrefixOf` option)

findBuiltInAutoCompleteMatch :: String -> WasAutoCompleteMatchFound
findBuiltInAutoCompleteMatch partialCommand = findBuiltInAutoCompleteMatch' partialCommand ["exit", "echo"]

findBuiltInAutoCompleteMatch' :: String -> [String] -> WasAutoCompleteMatchFound
findBuiltInAutoCompleteMatch' partialCommand builtins =
  let filteredBuiltins = filter (doesPartialCommandMatchBuiltin partialCommand) builtins
   in case filteredBuiltins of
        [] -> NoAutoCompleteMatchFound
        [x] -> AutoCompleteMatchFound ((head filteredBuiltins) ++ " ")
        _ -> AutoCompleteMatchesFound filteredBuiltins

doesPartialCommandMatchBuiltin :: String -> String -> Bool
doesPartialCommandMatchBuiltin partialCommand builtin = partialCommand /= "" && partialCommand `isPrefixOf` builtin

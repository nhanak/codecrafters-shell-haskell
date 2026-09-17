module Autocomplete.Core (getFileNameFromInputSoFar, getAutoCompletionType, AutoCompletionType (..), WasAutoCompleteMatchFound (..), findLongestCommonPrefix, findBuiltInAutoCompleteMatch, findAutoCompleteMatch, getFileNameAutoCompletionType, FileNameAutoCompletionType (..), getFileNameFromPartialNestedFileName, getPathFromPartialNestedFileName, getInputBeforeFilePath) where

import Data.List (intercalate, isInfixOf, isPrefixOf, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.FilePath (pathSeparator)

data WasAutoCompleteMatchFound = NoAutoCompleteMatchFound | AutoCompleteMatchFound String | AutoCompleteMatchesFound [String] deriving (Show)

data AutoCompletionType = CommandAutoCompletion | FileNameAutoCompletion deriving (Show)

data FileNameAutoCompletionType = NonNestedFileNameAutoCompletion | NestedFileNameAutoCompletion deriving (Show)

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
findBuiltInAutoCompleteMatch partialCommand = findAutoCompleteMatch partialCommand ["exit", "echo"]

findAutoCompleteMatch :: String -> [String] -> WasAutoCompleteMatchFound
findAutoCompleteMatch partial options =
  let filteredOptions = filter (doesPartialMatchOption partial) options
   in case filteredOptions of
        [] -> NoAutoCompleteMatchFound
        [x] -> AutoCompleteMatchFound ((head filteredOptions) ++ " ")
        _ -> AutoCompleteMatchesFound filteredOptions

doesPartialMatchOption :: String -> String -> Bool
doesPartialMatchOption partial option = partial /= "" && partial `isPrefixOf` option

getAutoCompletionType :: String -> AutoCompletionType
getAutoCompletionType inputSoFar = if length (words inputSoFar) == 1 && (last inputSoFar) /= ' ' then CommandAutoCompletion else FileNameAutoCompletion

getFileNameAutoCompletionType :: String -> FileNameAutoCompletionType
getFileNameAutoCompletionType inputSoFar = if pathSeparator `elem` last (words inputSoFar) then NestedFileNameAutoCompletion else NonNestedFileNameAutoCompletion

getFileNameFromInputSoFar :: String -> String
getFileNameFromInputSoFar inputSoFar = if length (words inputSoFar) < 2 then "" else last $ (words inputSoFar)

getFileNameFromPartialNestedFileName :: String -> String
getFileNameFromPartialNestedFileName partialNestedFileName = last $ splitOn [pathSeparator] partialNestedFileName

getPathFromPartialNestedFileName :: String -> String
getPathFromPartialNestedFileName partialNestedFileName = intercalate [pathSeparator] (init $ splitOn [pathSeparator] partialNestedFileName)

getInputBeforeFilePath :: String -> String
getInputBeforeFilePath inputSoFar = if (length (words inputSoFar)) == 1 then head (words inputSoFar) else unwords (init $ words inputSoFar)

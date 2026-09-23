module Autocomplete.Core
  ( onlyOneOptionMatchesPrefix,
    addSpaceIfNotDirectory,
    pathIsDirectoryLike,
    getFileNameFromInputSoFar,
    getAutoCompletionType,
    AutoCompletionType (..),
    WasAutoCompleteMatchFound (..),
    findLongestCommonPrefix,
    findBuiltInAutoCompleteMatch,
    findAutoCompleteMatch,
    getFileNameAutoCompletionType,
    FileNameAutoCompletionType (..),
    getFileNameFromPartialNestedFileName,
    getPathFromPartialNestedFileName,
    getInputBeforeFilePath,
    breakInputSoFarIntoCompleterScriptArgs,
    safeInit,
    initOrHead,
    getStringByteLength,
    outputSpansMultipleLines,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import Data.List (intercalate, isInfixOf, isPrefixOf, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.FilePath (pathSeparator)

data WasAutoCompleteMatchFound = NoAutoCompleteMatchFound | AutoCompleteMatchFound String | AutoCompleteMatchesFound [String] deriving (Show)

data AutoCompletionType = CommandAutoCompletion | FileNameAutoCompletion | CompleterScriptAutoCompletion deriving (Show)

data FileNameAutoCompletionType = NonNestedFileNameAutoCompletion | NestedFileNameAutoCompletion deriving (Show)

count :: (Eq a) => a -> [a] -> Int
count x xs = length (filter (== x) xs)

outputSpansMultipleLines :: String -> Bool
outputSpansMultipleLines out = count '\n' out > 1

getStringByteLength :: String -> Int
getStringByteLength str = B.length (BSU.fromString str)

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

onlyOneOptionMatchesPrefix :: String -> [String] -> Bool
onlyOneOptionMatchesPrefix prefix options = length (filter (\option -> prefix `isPrefixOf` option) options) == 1

findBuiltInAutoCompleteMatch :: String -> WasAutoCompleteMatchFound
findBuiltInAutoCompleteMatch partialCommand = findAutoCompleteMatch partialCommand ["exit", "echo"]

findAutoCompleteMatch :: String -> [String] -> WasAutoCompleteMatchFound
findAutoCompleteMatch partial options =
  let filteredOptions = filter (doesPartialMatchOption partial) options
   in case filteredOptions of
        [] -> NoAutoCompleteMatchFound
        [x] -> AutoCompleteMatchFound (addSpaceIfNotDirectory x)
        _ -> AutoCompleteMatchesFound filteredOptions

doesPartialMatchOption :: String -> String -> Bool
doesPartialMatchOption partial option = partial /= "" && partial `isPrefixOf` option

getAutoCompletionType :: String -> [String] -> AutoCompletionType
getAutoCompletionType inputSoFar completerScriptCommands
  | length (words inputSoFar) > 0 && isCompleterScriptCommand (head $ words inputSoFar) completerScriptCommands = CompleterScriptAutoCompletion
  | length (words inputSoFar) == 1 && (last inputSoFar) /= ' ' = CommandAutoCompletion
  | otherwise = FileNameAutoCompletion

breakInputSoFarIntoCompleterScriptArgs :: String -> [String]
breakInputSoFarIntoCompleterScriptArgs inputSoFar = case words inputSoFar of
  [arg1, arg2, arg3] -> [arg1, arg3, arg2]
  [arg1, arg3] -> [arg1, arg3, ""]
  _ -> []

isCompleterScriptCommand :: String -> [String] -> Bool
isCompleterScriptCommand command completerScriptCommands = command `elem` completerScriptCommands

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

safeInit :: String -> String
safeInit str = if length str == 0 then "" else init str

initOrHead :: [a] -> [a]
initOrHead lister = if length lister < 2 then lister else init lister

addSpaceIfNotDirectory :: String -> String
addSpaceIfNotDirectory path = if last path /= pathSeparator then path ++ " " else path

pathIsDirectoryLike :: String -> Bool
pathIsDirectoryLike path = pathSeparator `elem` path

module ArgsUtils (getCommand, getArgs) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Debug.Trace (traceShow)

getCommand :: String -> String
getCommand args = head (words args)

getArgs :: String -> [String]
getArgs argsWithCommand = filter (/= "") (tokenize $ (replaceDouble '\'' . replaceDouble '\"') (trim (getArgsWithoutCommand argsWithCommand)))

getArgsWithoutCommand :: String -> String
getArgsWithoutCommand args =
  let (first, rest) = break (== ' ') args
   in trim rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

mapEveryOther :: (a -> [a]) -> (a -> [a]) -> [a] -> [[a]]
mapEveryOther f g xs = zipWith ($) (cycle [f, g]) xs

breakArgsOnSingleQuotes :: String -> [String]
breakArgsOnSingleQuotes args = filter (/= "") (concat (mapEveryOther (\x -> words x) (\y -> [y]) (getAllSubstrings'' (replaceDouble '\'' $ trim args))))

-- breakArgsOnSingleQuotes args = filter (/= "") (concat (mapEveryOther (\x -> words x) (\y -> [y]) (getAllSubstrings'' (replaceDouble '\'' $ trim args))))
breakArgsOnDoubleQuotes :: String -> [String]
breakArgsOnDoubleQuotes args = getAllSubstrings' '"' (replaceDouble '"' args)

replaceDouble :: Char -> String -> String
replaceDouble char [x] = [x]
replaceDouble char (x : y : xs) = if (x == char) && (y == char) then replaceDouble char xs else x : replaceDouble char (y : xs)
replaceDouble char null = ""

getAllSubstrings :: Char -> Char -> String -> [String]
getAllSubstrings _ _ [] = []
getAllSubstrings start end str =
  let initial = takeWhile (/= start) str
      remaining = safeTail (dropWhile (/= start) str)
   in [initial] ++ getAllSubstrings start end remaining

getAllSubstrings'' :: String -> [String]
getAllSubstrings'' [] = []
getAllSubstrings'' str =
  let (initial, remaining) = takeUntilStartReached str
   in initial : getAllSubstrings'' remaining

-- WIP: Trying to deal with back slash escaping for ' char... i dont think a simple filter works
-- I think we need to modify our take
takeUntilStartReached' :: Char -> TakeUntilStartReachedAcc -> TakeUntilStartReachedAcc
takeUntilStartReached' cur acc =
  if _done acc || null (_remaining acc)
    then acc
    else case (prev, cur) of
      ('\\', '\\') -> TakeUntilStartReachedAcc {_done = False, _prev = ' ', _initial = _initial acc ++ [cur], _remaining = remaining'}
      ('\\', _) -> TakeUntilStartReachedAcc {_done = False, _prev = cur, _initial = _initial acc ++ [cur], _remaining = remaining'}
      (_, '\\') -> TakeUntilStartReachedAcc {_done = False, _prev = '\\', _initial = _initial acc, _remaining = remaining'}
      (_, '\'') -> TakeUntilStartReachedAcc {_done = True, _prev = cur, _initial = _initial acc, _remaining = remaining'}
      (_, _) -> TakeUntilStartReachedAcc {_done = False, _prev = cur, _initial = _initial acc ++ [cur], _remaining = remaining'}
  where
    prev = _prev acc
    remaining' = safeTail (_remaining acc)

data TakeUntilStartReachedAcc = TakeUntilStartReachedAcc {_done :: Bool, _prev :: Char, _initial :: String, _remaining :: String} deriving (Show)

takeUntilStartReached :: String -> (String, String)
takeUntilStartReached str =
  let acc = foldr takeUntilStartReached' (TakeUntilStartReachedAcc {_done = False, _prev = ' ', _initial = "", _remaining = str}) str
   in (reverse $ _initial acc, _remaining acc)

takeUntilEnclosed :: String -> String -> Bool -> (String, String)
takeUntilEnclosed remaining enclosed seenCloseChar =
  if null remaining || (seenCloseChar && head remaining == ' ')
    then (enclosed, remaining)
    else case remaining of
      ('"' : rest) -> takeUntilEnclosed rest enclosed True
      _ -> takeUntilEnclosed (tail remaining) (enclosed ++ [head remaining]) seenCloseChar

getAllSubstrings' :: Char -> String -> [String]
getAllSubstrings' _ [] = []
getAllSubstrings' start str =
  let initial = takeWhile (/= start) str
      (enclosed, remaining) = takeUntilEnclosed (safeTail (dropWhile (/= start) str)) "" False
   in [initial] ++ [enclosed] ++ getAllSubstrings' start remaining

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

-- WIP
replaceSpecialCharacters :: String -> String
replaceSpecialCharacters [] = []
replaceSpecialCharacters (x : y : ys) = case x of
  '\\' -> traceShow ("[DEBUG]: ", x, y, ys) ([y] ++ (replaceSpecialCharacters ys))
  _ -> [x] ++ (replaceSpecialCharacters ([y] ++ ys))

data TokenizerState = Normal | SingleQuotes | DoubleQuotesOpen | DoubleQuotesClose | Escape

data TokenizerAcc = TokenizerAcc {_tokenizerState :: TokenizerState, _curToken :: String, _argsList :: [String]}

tokenize :: String -> [String]
tokenize args =
  let tokenizerAcc = foldl tokenCombiner (TokenizerAcc {_tokenizerState = Normal, _curToken = "", _argsList = []}) args
      curToken = _curToken tokenizerAcc
      tokenizedArgs = _argsList tokenizerAcc
   in if null curToken then tokenizedArgs else tokenizedArgs ++ [curToken]

tokenCombiner :: TokenizerAcc -> Char -> TokenizerAcc
tokenCombiner acc char = case (_tokenizerState acc) of
  Normal -> handleNormalTokenizerState acc char
  SingleQuotes -> handleSingleQuotesTokenizerState acc char
  DoubleQuotesOpen -> handleDoubleQuotesOpenTokenizerState acc char
  DoubleQuotesClose -> handleDoubleQuotesCloseTokenizerState acc char
  Escape -> handleEscapeTokenizerState acc char

handleNormalTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleNormalTokenizerState acc char = case char of
  '\\' -> TokenizerAcc {_tokenizerState = Escape, _curToken = _curToken acc, _argsList = _argsList acc}
  '\'' -> TokenizerAcc {_tokenizerState = SingleQuotes, _curToken = "", _argsList = _argsList acc ++ [_curToken acc]}
  '\"' -> TokenizerAcc {_tokenizerState = DoubleQuotesOpen, _curToken = "", _argsList = _argsList acc ++ [_curToken acc]}
  ' ' -> TokenizerAcc {_tokenizerState = Normal, _curToken = "", _argsList = _argsList acc ++ [_curToken acc]}
  _ -> TokenizerAcc {_tokenizerState = Normal, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

handleEscapeTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleEscapeTokenizerState acc char = TokenizerAcc {_tokenizerState = Normal, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

handleSingleQuotesTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleSingleQuotesTokenizerState acc char = case char of
  '\'' -> TokenizerAcc {_tokenizerState = Normal, _curToken = "", _argsList = _argsList acc ++ [_curToken acc]}
  _ -> TokenizerAcc {_tokenizerState = SingleQuotes, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

handleDoubleQuotesOpenTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleDoubleQuotesOpenTokenizerState acc char = case char of
  '\"' -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc, _argsList = _argsList acc}
  _ -> TokenizerAcc {_tokenizerState = DoubleQuotesOpen, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

handleDoubleQuotesCloseTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleDoubleQuotesCloseTokenizerState acc char = case char of
  ' ' -> TokenizerAcc {_tokenizerState = Normal, _curToken = "", _argsList = _argsList acc ++ [_curToken acc]}
  _ -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

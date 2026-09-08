module ArgsUtils (getCommand, getArgs) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Debug.Trace (traceShow)

getCommand :: String -> String
getCommand args = head (words args)

getArgs :: String -> [String]
getArgs argsWithCommand = filter (/= "") (tokenize $ (replaceDouble '\'' . replaceDouble '\"') (trim $ getArgsWithoutCommand argsWithCommand))

getArgsWithoutCommand :: String -> String
getArgsWithoutCommand args =
  let (first, rest) = break (== ' ') args
   in trim rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

replaceDouble :: Char -> String -> String
replaceDouble char [x] = [x]
replaceDouble char (x : y : xs) = if (x == char) && (y == char) then replaceDouble char xs else x : replaceDouble char (y : xs)
replaceDouble char null = ""

data TokenizerState = Normal | SingleQuotes | DoubleQuotesOpen | DoubleQuotesClose | DoubleQuotesOpenBackslash | DoubleQuotesCloseBackslash | Escape

data TokenizerAcc = TokenizerAcc {_tokenizerState :: TokenizerState, _curToken :: String, _argsList :: [String]}

tokenize :: String -> [String]
tokenize args =
  let tokenizerAcc = foldl tokenCombiner (TokenizerAcc {_tokenizerState = Normal, _curToken = "", _argsList = []}) args
      curToken = _curToken tokenizerAcc
      tokenizedArgs = _argsList tokenizerAcc
   in if null curToken then tokenizedArgs else tokenizedArgs ++ [curToken]

tokenCombiner :: TokenizerAcc -> Char -> TokenizerAcc
tokenCombiner acc char = case _tokenizerState acc of
  Normal -> handleNormalTokenizerState acc char
  SingleQuotes -> handleSingleQuotesTokenizerState acc char
  DoubleQuotesOpen -> handleDoubleQuotesOpenTokenizerState acc char
  DoubleQuotesOpenBackslash -> handleDoubleQuotesOpenBackslashTokenizerState acc char
  DoubleQuotesCloseBackslash -> handleDoubleQuotesCloseBackslashTokenizerState acc char
  DoubleQuotesClose -> handleDoubleQuotesCloseTokenizerState acc char
  Escape -> handleEscapeTokenizerState acc char

handleNormalTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleNormalTokenizerState acc char = case char of
  '\\' -> TokenizerAcc {_tokenizerState = Escape, _curToken = _curToken acc, _argsList = _argsList acc}
  '\'' -> TokenizerAcc {_tokenizerState = SingleQuotes, _curToken = _curToken acc, _argsList = _argsList acc}
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
  '\\' -> TokenizerAcc {_tokenizerState = DoubleQuotesOpenBackslash, _curToken = _curToken acc, _argsList = _argsList acc}
  '\"' -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc, _argsList = _argsList acc}
  _ -> TokenizerAcc {_tokenizerState = DoubleQuotesOpen, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

handleDoubleQuotesOpenBackslashTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleDoubleQuotesOpenBackslashTokenizerState acc char = case char of
  '\"' -> TokenizerAcc {_tokenizerState = DoubleQuotesOpen, _curToken = _curToken acc ++ ['\"'], _argsList = _argsList acc}
  '\\' -> TokenizerAcc {_tokenizerState = DoubleQuotesOpen, _curToken = _curToken acc ++ ['\\'], _argsList = _argsList acc}
  _ -> TokenizerAcc {_tokenizerState = DoubleQuotesOpen, _curToken = _curToken acc ++ ['\\', char], _argsList = _argsList acc}

handleDoubleQuotesCloseBackslashTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleDoubleQuotesCloseBackslashTokenizerState acc char = case char of
  '\"' -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc ++ ['\"'], _argsList = _argsList acc}
  '\\' -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc ++ ['\\'], _argsList = _argsList acc}
  _ -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc ++ ['\\', char], _argsList = _argsList acc}

handleDoubleQuotesCloseTokenizerState :: TokenizerAcc -> Char -> TokenizerAcc
handleDoubleQuotesCloseTokenizerState acc char = case char of
  '\\' -> TokenizerAcc {_tokenizerState = DoubleQuotesCloseBackslash, _curToken = _curToken acc, _argsList = _argsList acc}
  ' ' -> TokenizerAcc {_tokenizerState = Normal, _curToken = "", _argsList = _argsList acc ++ [_curToken acc]}
  _ -> TokenizerAcc {_tokenizerState = DoubleQuotesClose, _curToken = _curToken acc ++ [char], _argsList = _argsList acc}

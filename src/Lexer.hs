import Data.Maybe
import Data.List

data AST = Leaf Int | Add AST AST | Sub AST AST
  deriving Show

data Token = Literal String | LParen | RParen | OpAdd | OpSub | LexerError
  deriving Show

type LexResult = Maybe (Token, String)

lexedToken :: (Token, String) -> Token
lexedToken (a, _) = a

lexedRest :: (Token, String) -> String
lexedRest (_, a) = a

readSimpleToken :: Token -> String -> (String -> LexResult)
readSimpleToken token repr from
  | isJust(rest) = Just (token, (fromJust rest))
  | otherwise = Nothing
  where rest = stripPrefix repr from

tryReadAll :: [(String -> LexResult)] -> String -> [LexResult]
tryReadAll [] from = []
tryReadAll (reader:readers) from = (reader from):(tryReadAll readers from)

selectFirst :: [LexResult] -> LexResult
selectFirst ((Nothing):[]) = Nothing
selectFirst ((Nothing):xs) = selectFirst xs
selectFirst ((Just a):xs) = Just a

tokenize :: [(String -> LexResult)] -> String -> [Token]
tokenize _ [] = []
tokenize readers (' ':xs) = tokenize readers xs
tokenize readers from
  | isJust(match) = (lexedToken . fromJust $ match):(tokenize readers (lexedRest . fromJust $ match))
  | otherwise = [LexerError]
  where match = selectFirst . (tryReadAll readers) $ from

readLParen = readSimpleToken LParen "("
readRParen = readSimpleToken RParen ")"
readOpAdd = readSimpleToken OpAdd "Add"
readOpSub = readSimpleToken OpSub "Sub"
readers = [readLParen, readRParen, readOpSub, readOpAdd]

main = print (tokenize readers "((Add Sub))")
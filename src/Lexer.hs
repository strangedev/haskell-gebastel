import Data.Maybe
import Data.List
import Data.Char

data AST = Leaf Int | Add AST AST | Sub AST AST
  deriving Show

data Token = Literal String | LParen String | RParen String | OpAdd String | OpSub String
  deriving Show

type LexResult = Maybe (Token, String)

lexedToken :: (Token, String) -> Token
lexedToken (a, _) = a

lexedRest :: (Token, String) -> String
lexedRest (_, a) = a

matchGreedy _ [] = Nothing
matchGreedy matcher (x:xs) = matchLeft matcher [x] xs

data MatchingResult = Match | NoMatch | Continue
  deriving Show

-- left, lookahead, result
type LookaheadMatcher = (String -> Maybe Char -> MatchingResult)

-- match prefixes of the second argument and check if they
-- satisfy a matcher.
matchLeft :: LookaheadMatcher -> String -> String -> Maybe (String, String)
matchLeft matcher left []     = case matcher left Nothing of
  Continue  -> Nothing
  NoMatch   -> Nothing
  Match     -> Just (left, [])
matchLeft matcher left (x:xs) = case matcher left (Just x) of
  NoMatch   -> Nothing
  Match     -> Just (left, (x:xs))
  Continue  -> matchLeft matcher (left ++ [x]) xs

readToken :: (String -> Token) -> LookaheadMatcher -> String -> Maybe (Token, String)
readToken ctor matcher input = case matchGreedy matcher input of
  Just (prefix, suffix) -> Just (ctor prefix, suffix)
  Nothing               -> Nothing

literalMatch :: String -> String -> Maybe Char -> MatchingResult
literalMatch pattern input _
  | pattern == input              = Match
  | length input < length pattern = Continue
  | otherwise                     = NoMatch

tryReadAll :: [(String -> LexResult)] -> String -> [LexResult]
tryReadAll [] from = []
tryReadAll (reader:readers) from = (reader from):(tryReadAll readers from)

selectFirst :: [LexResult] -> LexResult
selectFirst ((Nothing):[]) = Nothing
selectFirst ((Nothing):xs) = selectFirst xs
selectFirst ((Just a):xs) = Just a

data TokenStream = Tokens [Token] | LexerError [Token] String
  deriving Show

tokenize :: [(String -> LexResult)] -> String -> TokenStream
tokenize readers [] = LexerError [] ""
tokenize readers input    = tokenizeHelper [] input 
  where tokenizeHelper :: [Token] -> String -> TokenStream
        tokenizeHelper acc (' ':xs) = tokenizeHelper acc xs
        tokenizeHelper acc []       = Tokens acc
        tokenizeHelper acc input    = case selectFirst . (tryReadAll readers) $ input of
          Just match  -> tokenizeHelper (acc ++ [lexedToken match]) (lexedRest match)
          Nothing     -> LexerError acc input


isNumberString :: String -> Bool
isNumberString = foldl (&&) True . map isDigit

matchNumber :: LookaheadMatcher
matchNumber input Nothing
  | isNumberString input  = Match
  | otherwise             = NoMatch
matchNumber input (Just c)
  | (isNumberString input) && (isDigit c)     = Continue
  | (isNumberString input) && not (isDigit c) = Match
  | otherwise                                 = NoMatch

readLParen = readToken LParen (literalMatch "(")
readRParen = readToken RParen (literalMatch ")")
readOpAdd = readToken OpAdd (literalMatch "Add")
readOpSub = readToken OpSub (literalMatch "Sub")
readLiteral = readToken Literal matchNumber
readers = [readLParen, readRParen, readOpSub, readOpAdd, readLiteral]

lexMaths = tokenize readers

main = print (lexMaths "(Sub (Add 354 332) 221)")
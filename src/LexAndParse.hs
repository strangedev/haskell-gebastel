{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Maybe
import Data.List
import Data.Char
import Text.Read

data Token = LiteralValue String | LeftParenthese String | RightParenthese String | OperatorAdd String | OperatorSub String | OperatorMult String
  deriving Show

-- A TokenLexer lexes a certain Token from the left of a String.
-- Applying a TokenLexer to a string either returns the Token
-- that prefixes the String, or Nothing, if the Token is not a
-- prefix of the String.
type LexResult = Maybe (Token, String)
type TokenLexer = String -> LexResult

lexedToken :: (Token, String) -> Token
lexedToken (a, _) = a

lexedRest :: (Token, String) -> String
lexedRest (_, a) = a

-- In order to determine if a Token prefixes a String, we use a matcher, that examines
-- a prefix of the string with one char lookahead.
-- We start with the smallest prefix and Continue consuming until we have a Match or NoMatch.
data MatchingResult = Match | NoMatch | Continue
  deriving Show

-- The matcher looks at a prefix and the next Char following it.
type LookaheadMatcher = (String -> Maybe Char -> MatchingResult)

-- This function applies the matcher to an input. If some prefix 
-- matches, it returns the prefix and the rest of the input.
-- If the matcher return NoMatch at any point, it returns Nothing.
matchGreedy :: LookaheadMatcher -> String -> Maybe (String, String)
matchGreedy _ [] = Nothing
matchGreedy matcher (x:xs) = matchLeft matcher [x] xs

matchLeft :: LookaheadMatcher -> String -> String -> Maybe (String, String)
matchLeft matcher left []     = case matcher left Nothing of
  Continue  -> Nothing
  NoMatch   -> Nothing
  Match     -> Just (left, [])
matchLeft matcher left (x:xs) = case matcher left (Just x) of
  NoMatch   -> Nothing
  Match     -> Just (left, (x:xs))
  Continue  -> matchLeft matcher (left ++ [x]) xs

-- This is how we create new TokenLexers.
-- We supply a constructor and a LookaheadMatcher for the Token we wish to
-- lex. Applied to an input, the TokenLexer applies the greedy matching
-- to it. If a match is found, it is returned, otherwise we return Nothing.
readToken :: (String -> Token) -> LookaheadMatcher -> TokenLexer
readToken ctor matcher input = case matchGreedy matcher input of
  Just (prefix, suffix) -> Just (ctor prefix, suffix)
  Nothing               -> Nothing


-- We can now read a Token from the left of some String by applying
-- a TokenLexer to it. If the string is prefixed with the Token,
-- we get Just the Token.
-- How do we determine __which__ token prefixes a given String?
-- We just apply all TokenLexers to the String and select the first
-- one that doesn't return Nothing.

-- This is some sort of "reverse map" to apply a List of TokenLexers to
-- the same input.
tryReadAll :: [TokenLexer] -> String -> [LexResult]
tryReadAll [] from = []
tryReadAll (reader:readers) from = (reader from):(tryReadAll readers from)

-- And this selects the first non-empty LexResult from that List.
selectFirst :: [LexResult] -> LexResult
selectFirst ((Nothing):[]) = Nothing
selectFirst ((Nothing):xs) = selectFirst xs
selectFirst ((Just a):xs) = Just a

-- Now all that is left is to apply this approach repeatedly to the input String,
-- decrreasing the size of the remaining String with each matched Token.
-- This creates a List of Tokens, the TokenStream.
-- If, at any point, all of the TokenLexers return an empty LexResult,
-- we have encountered an error in the syntax.
-- In that case, the remainder of the input String is retruned also.
data TokenStream = Tokens [Token] | LexerError [Token] String
  deriving Show

tokenize :: [TokenLexer] -> String -> TokenStream
tokenize readers [] = LexerError [] ""
tokenize readers input    = tokenizeHelper [] input 
  where tokenizeHelper :: [Token] -> String -> TokenStream
        tokenizeHelper acc (' ':xs) = tokenizeHelper acc xs
        tokenizeHelper acc []       = Tokens acc
        tokenizeHelper acc input    = case selectFirst . (tryReadAll readers) $ input of
          Just match  -> tokenizeHelper (acc ++ [lexedToken match]) (lexedRest match)
          Nothing     -> LexerError acc input

-- Now to applying this practically.

-- For our Tokens defined in the beginning, we have to create some
-- LookaheadMatchers.

-- For verbatim matches of some String prefix, we can just continue expanding
-- the prefix until it is of the correct length.
-- Then, we just compare the two strings. We ignore the lookahead.
literalMatch :: String -> LookaheadMatcher
literalMatch pattern input _
  | length input < length pattern = Continue 
  | pattern == input              = Match
  | otherwise                     = NoMatch

-- For matching numbers, we have to use lookahead.
-- This is because we do not know how long the number
-- will be. We would only know that the number had ended,
-- if we encountered a non-digit. At that point, matching
-- would consume the next Char of the input also.
-- For this reason, we look at once Char to the left
-- of the prefix to determine if the number has ended.
isNumberString :: String -> Bool
isNumberString = foldl (&&) True . map isDigit

-- Note that the lookahead might not always exist!
-- Consider a number spanning the last few Chars of the
-- input. There, the number is not delimited by another
-- Char, but Nothing.
matchNumber :: LookaheadMatcher
matchNumber input Nothing
  | isNumberString input  = Match
  | otherwise             = NoMatch
matchNumber input (Just c)
  | (isNumberString input) && (isDigit c)     = Continue
  | (isNumberString input) && not (isDigit c) = Match
  | otherwise                                 = NoMatch

-- We may now create some TokenLexers from the LookaheadMatchers
readLeftParenthese = readToken LeftParenthese (literalMatch "(")
readRightParenthese = readToken RightParenthese (literalMatch ")")
readOperatorAdd = readToken OperatorAdd (literalMatch "+")
readOperatorSub = readToken OperatorSub (literalMatch "-")
readOperatorMult = readToken OperatorMult (literalMatch "*")
readLiteralValue = readToken LiteralValue matchNumber
readers = [readLeftParenthese, readRightParenthese, readOperatorSub, readOperatorAdd, readLiteralValue, readOperatorMult]

-- And finally, this is our Lexer
lexMaths :: String -> TokenStream
lexMaths = tokenize readers

-- HELO DIS IS PARSER

data AST =
  Value Int   |
  Add AST AST |
  Sub AST AST |
  Mult AST AST
  deriving Show

data Symbol = Atom Token | Reduced AST
  deriving Show

type Reducer = [Symbol] -> Maybe [Symbol]

reduceParens :: Reducer
reduceParens (
    (Atom (LeftParenthese _)):
    (Reduced a):
    (Atom (RightParenthese _)):
    xs
  ) = Just $ (Reduced a):xs 
reduceParens _ = Nothing

reduceAdd :: Reducer
reduceAdd (
    (Reduced a):
    (Atom (OperatorAdd _)):
    (Reduced b):
    xs
  ) = Just $ (Reduced (Add a b)):xs
reduceAdd _ = Nothing

reduceSub :: Reducer
reduceSub (
    (Reduced a):
    (Atom (OperatorSub _)):
    (Reduced b):
    xs
  ) = Just $ (Reduced (Sub a b)):xs
reduceSub _ = Nothing

reduceMult :: Reducer
reduceMult (
    (Reduced a):
    (Atom (OperatorMult _)):
    (Reduced b):
    xs
  ) = Just $ (Reduced (Mult a b)):xs
reduceMult _ = Nothing

reduceValue :: Reducer
reduceValue (
    (Atom (LiteralValue a)):
    xs
  ) = case readMaybe a :: Maybe Int of
  Just x  -> Just $ (Reduced (Value x)):xs
  Nothing -> Nothing
reduceValue _ = Nothing

instance Show Reducer where
  show a = "SomeReducer"

-- reducers, input, output
data ParserState = ParserState [Reducer] [Symbol] [Symbol]
  deriving Show

tryReductions :: ParserState -> [ParserState]
tryReductions (ParserState reducers input output) = reducedParserStates
  where reductions = map ($ output) reducers
        validReductions = map fromJust . filter isJust $ reductions
        reducedParserStates = map (ParserState reducers input) validReductions

shift :: ParserState -> Maybe ParserState
shift (ParserState _ [] _) = Nothing
shift (ParserState reducers (x:xs) output) = Just $ ParserState reducers xs (x:output)

-- a ParserState is terminated when the entire input was read and only one
-- symbol remains in the output, i.e. it is fully reduced
isTerminated :: ParserState -> Bool
isTerminated (ParserState _ [] (symbol:[])) = True
isTerminated _ = False

shiftReduceStep :: ParserState -> [ParserState]
shiftReduceStep parser = case tryReductions parser of
  -- no reductions possible
  [] -> maybeToList $ shift parser
  -- some reductions possible 
  reducedParserStates -> reducedParserStates

shiftReduce :: [ParserState] -> Maybe Symbol
shiftReduce parsers = case find isTerminated parsers of
  Just (ParserState _ _ (x:xs))  -> Just x
  Nothing                   -> case map shiftReduceStep parsers of
    []  -> Nothing
    xs  -> shiftReduce $ concat xs

fromReducers :: [Reducer] -> [Symbol] -> [ParserState]
fromReducers reducers input = [ParserState reducers input []]

fromTokens :: [Token] -> [Symbol]
fromTokens = map Atom

fromTokenStream :: TokenStream -> [Symbol]
fromTokenStream (Tokens t) = reverse . fromTokens $ t

parseMaths = shiftReduce . fromReducers [reduceParens, reduceValue, reduceAdd, reduceSub, reduceMult]

fromSymbol :: Symbol -> AST
fromSymbol (Reduced a) = a

execute :: AST -> Int
execute (Value a) = a
execute (Add a b) = (execute a) + (execute b)
execute (Sub a b) = (execute a) - (execute b)
execute (Mult a b) = (execute a) * (execute b)

calcStuffs = execute . fromSymbol . fromJust . parseMaths . fromTokenStream . lexMaths
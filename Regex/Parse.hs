module Regex.Parse (regexFromString) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Data.List
import Regex.Types


parseRegex  :: Parser Rx
parseRegex = choice [ try parsePipes, try parseConcat, try parseStar, try parseParens, try parseAlphaNum ]

parsePipes :: Parser Rx 
parsePipes = do 
    r1 <- parseRegex'
    char '|'
    r2 <- parseRegex
    return $ Union r1 r2
    where parseRegex' = choice [ try parseConcat, try parseStar, try parseParens, try parseAlphaNum ]

parseConcat :: Parser Rx
parseConcat = do
    r1 <- parseRegex'
    r2 <- parseRegex''
    return $ Concat r1 r2
    where parseRegex'  = choice [ try parseStar, try parseParens, try parseAlphaNum ]
          parseRegex'' = choice [ try parseConcat, try parseStar, try parseParens, try parseAlphaNum ]

parseStar :: Parser Rx
parseStar = do 
    r <- parseRegex'
    char '*'
    return $ Star r
    where parseRegex' = choice [ try parseParens, try parseAlphaNum ]

parseParens :: Parser Rx
parseParens = do
    char '(' 
    a <- parseRegex 
    char ')'
    return a

parseAlphaNum :: Parser Rx
parseAlphaNum = do
    c <- alphaNum
    return $ Single c

regexFromString :: String -> Maybe Rx
regexFromString str = case (parse parseRegex "Failed to parse regex." str) of 
    Right s   -> Just s
    otherwise -> Nothing

-- main = print $ regexFromString "a(b)*|(cd|e*f*)*"
module Regex.Types where 

data Rx = Empty | Single Char | Star Rx | Union Rx Rx | Concat Rx Rx deriving (Show)
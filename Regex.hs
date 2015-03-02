module Regex (matchRegexStr) where 

import Regex.Parse
import Regex.Build
import Regex.Types

matchRegexStr :: String -> String -> Maybe Bool
matchRegexStr rx str = case regexFromString rx of 
    Just regEx -> Just $ matchRegex regEx str
    otherwise  -> Nothing

-- main = print $ matchRegexStr "(AB|CD)*(EF|G)*" "ABABABEFEFEF"
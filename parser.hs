module Parser where

import Text.Trifecta

parseInt :: String -> Result Integer
parseInt = parseString (do
    i <- integer 
    eof 
    return i) mempty
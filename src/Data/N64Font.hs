module Data.N64Font where


chars = "NUL" : take 14 (repeat "N/A") ++ [" "] ++ map show [0..9] ++ map show ['A'..'Z'] ++ repeat "TODO"

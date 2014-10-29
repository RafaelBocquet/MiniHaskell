module Backend.Mangle where

import Syntax.Name

import Control.Applicative

mangleMap = zip "!#$%&*+./<=>?@\\^|-~:_" ['a'..]

mangleChar :: Char -> String
mangleChar c = case lookup c mangleMap of
  Nothing -> [c]
  Just c' -> ['_', '_', c']

mangleSingle :: String -> String
mangleSingle = concat . fmap mangleChar

mangle :: QCoreName -> String
mangle (QName md _ (CoreName i n)) = (concat $ (++ "_") . mangleSingle <$> md) ++ mangleSingle n ++ show i

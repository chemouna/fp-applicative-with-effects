module PatternVariations where

import Prelude hiding (sequence)
import Control.Monad hiding (sequence)

sequence :: [IO a] -> IO [a]
sequence [] = return []
sequence (c:cs) = do
  x <- c
  xs <- sequence cs
  return (x:xs)

sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (c:cs) = return (:) `ap` c `ap` sequence cs


transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)







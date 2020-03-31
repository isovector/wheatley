{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall   #-}

module Lens where


import Control.Lens
import qualified Data.Text as T
import qualified Data.List as List

type Line = Int
type Col = Int


txt :: T.Text
txt = "12345678\nmiddle\n12345678"

textSpan :: (Line, Col) -- ^ Start point (inclusive)
         -> (Line, Col) -- ^ End point (exclusive)
         -> Traversal' T.Text T.Text
textSpan (startLine, startCol) (endLine, endCol) =
    traverseOf ( iso T.lines T.unlines
               . overRange startLine endLine
               . restrictingColumns startCol endCol)
               . iso T.unlines T.lines

restrictingColumns :: Int -> Int -> Traversal' [T.Text]  [T.Text]
restrictingColumns _ _ f [] = f []
restrictingColumns start end f [x] =
  let (prefix, remainder) = T.splitAt start x
      (remainder', suffix) = T.splitAt (end - start) remainder
   in (\middle -> middle & _head %~ (prefix <>) & _last <>~ suffix) <$> f [remainder']
restrictingColumns start end f (firstLine : remainder) =
  case unsnoc remainder of
    Nothing -> pure []
    Just (middleLines, lastLine) ->
      let (prefix, firstRemainder) = T.splitAt start firstLine
          (lastRemainder, suffix)  = T.splitAt end lastLine
       in (\middle -> middle & _head %~ (prefix <>) & _last <>~ suffix)
      <$> f (firstRemainder : (middleLines `snoc` lastRemainder))

overRange :: Int -> Int -> Traversal' [a] [a]
overRange start end f xs = do
    let (prefix, remainder)  = List.splitAt start xs
        (remainder', suffix) = List.splitAt ((end - start) + 1) remainder
     in (\xs' -> prefix <> xs' <> suffix) <$> (f remainder')


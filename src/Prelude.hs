module Prelude
  ( module BasePrelude
  , Text
  , pack
  ) where

import BasePrelude hiding
  ( (++)
  , lines
  , unlines
  , words
  , unwords
  , ShowS
  , showList
  , showString
  , Read (..)
  , reads
  , readParen
  , read
  , lex
  , putStr
  , getLine
  , getContents
  , interact
  , readFile
  , writeFile
  , appendFile
  , readIO
  , userError
  , fail
  , arr
  , try
  )
import Data.Text


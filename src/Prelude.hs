module Prelude
  ( module BasePrelude
  , Text
  , Map
  , pack
  ) where

import BasePrelude hiding
  ( (++)
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
  , typeOf
  )

import Data.Text
import Data.Map (Map)


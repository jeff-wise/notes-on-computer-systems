--
-- Assets
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}


module Data.Assets where


import Data.Icon (IconIndex)



newtype Assets = Assets {
  iconIndex :: IconIndex
} deriving (Eq, Show)




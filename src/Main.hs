--  
-- Main 
--  

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Yaml as Y

import Config (Config (..), ConfigFromFileErr (..), configFromFile)
import Data.Assets (Assets (..))
import Data.Icon
  ( IconIndex
  , iconSVGWithName, load
  )
import Web.Server as Web (runServer)



main :: IO ()
main = do
  putStrLn "Reading configuration: config.yaml"
  eConf <- configFromFile
  case eConf of
    Left (ConfigFromFileErr err) -> do
      print $ Y.prettyPrintParseException err
    Right conf -> do
      putStrLn "Loading assets: icons/"
      iconIndex <- load conf.iconsDir
      putStrLn $ "Starting web server: PORT=" ++ show conf.port
      Web.runServer conf $ Assets iconIndex

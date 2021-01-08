--  
-- Main 
--  
module Main where

import Web.Server (runServer)

main :: IO ()
main = do
  -- read config
  conf
  -- run server with config
  Web.runServer conf

--
-- Site Configuration
--
module Config where


data Config = Config {
    configPort :: Integer
  , articleDir :: String
}


-- read config from file

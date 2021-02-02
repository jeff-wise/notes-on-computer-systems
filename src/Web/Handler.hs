--
-- HTML: Page
--


-- {-# LANGUAGE OverloadedStrings #-}


module Web.Handler where


import Control.Monad.IO.Class (liftIO)

import Servant (Handler)
import Text.Blaze.Html (Html)

import Data.Assets (Assets)
import Data.Article (ArticleIndex (..), Article (..))
import Web.HTML.Page (pageHTML, pageIndexContentHTML)


pageIndex :: Assets -> Handler Html
pageIndex assets = do
  --liftIO $ putStrLn "here"
  return $ pageHTML assets $ pageIndexContentHTML $ ArticleIndex 
    [ Article "A Fake Blog Title in order to Fill the Space" "A short description for this one" "01/02/21"
    , Article "A blog about Front End Web Development" "How to create a simple website and learn front end development made easy, on the longer side" "11/07/20"
    , Article "Shorter Blog Name" "Do I really want? To have summaries for these?" "10/31/20"
    , Article "Maybe this is also a note" "Lets today look at and examine a really interesting topic" "09/09/20"
    ]



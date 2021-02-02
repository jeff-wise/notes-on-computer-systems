--
-- HTML: Page
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.Page (
    pageHTML, pageIndexContentHTML
  ) where


import Control.Monad (forM_)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)
import Data.Article
  ( ArticleIndex (..)
  , articleTitle, articleSummary, articleDatePublished
  )


-- * -> HTML
-- -----------------------------------------------------------------------------

pageHTML :: Assets -> Html -> Html
pageHTML assets contentHtml = H.docTypeHtml $ do
  H.head $ do
    H.title "Notes on Computer Systems"
    -- Fonts
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "https://cloud.typography.com/6602898/7018412/css/fonts.css"
    -- CSS: Page
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page.css"
    -- Alpine
    H.preEscapedString "<script src='https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.8.0/dist/alpine.min.js' defer></script>"
  H.body $ do
    H.div ! A.id "content" $ do
      H.div ! A.id "content-sidebar" $ contentSidebarHTML assets
      H.div ! A.id "content-main" $ contentHtml


contentSidebarHTML :: Assets -> Html
contentSidebarHTML assets = do
  H.div ! A.id "content-sidebar-header" $ do
    H.div ! A.id "content-sidebar-header-title" $ "notes on computer systems"
  H.div ! A.id "content-sidebar-nav" $ do
    H.div ! class_ "content-sidebar-nav-links" $ do
      H.span ! class_  "content-sidebar-nav-normal" $ "A collection of "
      H.span ! class_  "content-sidebar-nav-link-cat" $ do
        H.a "notes" 
      H.span ! class_  "content-sidebar-nav-normal" $ ","
      H.span ! class_  "content-sidebar-nav-link-type" $ do
        H.a "essays" 
      H.span ! class_  "content-sidebar-nav-normal" $ ","
      H.span ! class_  "content-sidebar-nav-link-type" $ do
        H.a "hypotheses," 
      H.span ! class_  "content-sidebar-nav-normal" $ "and"
      H.span ! class_  "content-sidebar-nav-link-type" $ do
        H.a "tutorials" 
      H.span ! class_  "content-sidebar-nav-normal" $ "on"
      H.span ! class_  "content-sidebar-nav-normal" $ "computers"
      H.span ! class_  "content-sidebar-nav-normal" $ "and"
      H.span ! class_  "content-sidebar-nav-normal" $ "their"
      H.span ! class_  "content-sidebar-nav-normal" $ "applications"
      H.span ! class_  "content-sidebar-nav-normal" $ "such as"
      H.span ! class_  "content-sidebar-nav-link-cat" $ do
        H.a "programming languages"
      H.span ! class_  "content-sidebar-nav-normal" $ ","
      H.span ! class_  "content-sidebar-nav-link-cat" $ do
        H.a "software development,"
      H.span ! class_  "content-sidebar-nav-normal" $ "and"
      H.span ! class_  "content-sidebar-nav-link-cat" $ do
        H.a "distributed systems"
      H.span ! class_  "content-sidebar-nav-normal" $ "."
  H.div ! A.id "content-sidebar-footer" $ do
    H.div ! class_ "content-sidebar-footer-section" $ do
      H.div ! class_ "content-sidebar-footer-section-icon user-typing" $ do
        let mIconText = iconSVGWithName "user-typing" assets.iconIndex
        case mIconText of
          Nothing       -> return () 
          Just iconText -> H.preEscapedString $ T.unpack iconText
      H.div ! class_ "content-sidebar-footer-section-title" $ "About the Authors"
    H.div ! class_ "content-sidebar-footer-section" $ do
      H.div ! class_ "content-sidebar-footer-section-icon book-shelf" $ do
        let mIconText = iconSVGWithName "book-shelf" assets.iconIndex
        case mIconText of
          Nothing       -> return () 
          Just iconText -> H.preEscapedString $ T.unpack iconText
      H.div ! class_ "content-sidebar-footer-section-title" $ "Recommended Reading"
    H.div ! class_ "content-sidebar-footer-section" $ do
      H.div ! class_ "content-sidebar-footer-section-icon chat-message" $ do
        let mIconText = iconSVGWithName "chat-message" assets.iconIndex
        case mIconText of
          Nothing       -> return () 
          Just iconText -> H.preEscapedString $ T.unpack iconText
      H.div ! class_ "content-sidebar-footer-section-title" $ "Send me a Message"
    H.div ! class_ "content-sidebar-footer-section" $ do
      H.div ! class_ "content-sidebar-footer-section-icon beer" $ do
        let mIconText = iconSVGWithName "beer" assets.iconIndex
        case mIconText of
          Nothing       -> return () 
          Just iconText -> H.preEscapedString $ T.unpack iconText
      H.div ! class_ "content-sidebar-footer-section-title" $ "Buy me a Beer"



pageIndexContentHTML :: ArticleIndex -> Html
pageIndexContentHTML (ArticleIndex articles) = do
  H.div ! class_ "page-index" $ do
    -- last updated
    H.div ! class_ "page-index-last-updated" $ do
      H.div ! class_ "page-index-last-updated-text" $ "Last updated Sunday, January 10th, 2021"
    -- toolbar
    H.div ! class_ "page-index-toolbar" $ do
      H.div ! class_ "page-index-toolbar-sort" $ do
        H.div ! class_ "page-index-toolbar-sort-button" $ "by series"
      H.div ! class_ "page-index-toolbar-header" $ do
        H.span ! class_ "page-index-toolbar-normal" $ "showing"
        H.span ! class_ "page-index-toolbar-count" $ "5"
        H.span ! class_ "page-index-toolbar-normal" $ "notes, essays, hypotheses, and tutorials"
    -- article list
    H.div ! class_ "page-index-article-list" $ do
      H.ul $ do
        forM_ articles $ \article -> do
          H.li $ do
            H.div ! class_ "page-index-article-sidebar" $ do
              H.div ! class_ "page-index-article-sidebar-content" $ 
                toHtml $ articleDatePublished article
            H.div ! class_ "page-index-article-main" $ do
              H.span ! class_ "page-index-article-title" $ 
                toHtml $ articleTitle article
              H.span ! class_ "page-index-article-summary" $ 
                toHtml $ articleSummary article

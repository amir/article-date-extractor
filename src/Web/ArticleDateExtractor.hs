{-# LANGUAGE OverloadedStrings #-}

module Web.ArticleDateExtractor
  (
    fromUrl,
    fromHtml
  ) where

import Data.Aeson
import Text.XML.HXT.Core
import Data.Char (toLower)
import Control.Applicative
import Data.Time (UTCTime)
import Network.Curl (curlGetString)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.HandsomeSoup hiding (fromUrl)
import Network.Curl.Opts (CurlOption (CurlFollowLocation))
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.ByteString.Lazy.Char8 as C

data ArticleContext = ArticleContext { datePublished :: String }
                      deriving (Show)

instance FromJSON ArticleContext where
  parseJSON (Object v)  = ArticleContext <$> v .: "datePublished"
  parseJSON _           = empty

readUrl :: String -> IO String
readUrl url = do
  (_, rsp) <- curlGetString url [CurlFollowLocation True]
  return rsp

hasAttrValue' :: ArrowXml a => String -> String -> a XmlTree XmlTree
hasAttrValue' a v = hasAttrValue a (map toLower >>> (== v))

extractFromLdJson doc = runX $ doc                  >>>
      css "script"                                  >>>
      hasAttrValue' "type" "application/ld+json"    >>>
      getChildren                                   >>>
      getText                                       >>.
      map C.pack

extractFromHead doc = runX $ doc >>> css "meta" >>>
  (
    hasAttrValue' "name"       "pubdate"                        <+>
    hasAttrValue' "name"       "publishdate"                    <+>
    hasAttrValue' "name"       "timestamp"                      <+>
    hasAttrValue' "name"       "dc.date.issued"                 <+>
    hasAttrValue' "property"   "article:published_time"         <+>
    hasAttrValue' "name"       "date"                           <+>
    hasAttrValue' "property"   "bt:pubdate"                     <+>
    hasAttrValue' "name"       "sailthru.date"                  <+>
    hasAttrValue' "name"       "article.published"              <+>
    hasAttrValue' "name"       "published-date"                 <+>
    hasAttrValue' "name"       "article.created"                <+>
    hasAttrValue' "name"       "article_date_original"          <+>
    hasAttrValue' "name"       "cxenseparse:recs:publishtime"   <+>
    hasAttrValue' "name"       "date_published"                 <+>
    hasAttrValue' "itemprop"   "datepublished"                  <+>
    hasAttrValue' "itemprop"   "datecreated"                    <+>
    hasAttrValue' "name"       "created"                        <+>
    hasAttrValue' "http-equiv" "date"
  ) >>> getAttrValue "content"

extractFromBody doc = runX $ doc >>>
  (
    (css "time" >>> getAttrValue "datetime")                                    <+>
    (css "span" >>> hasAttrValue "itemprop" (== "datePublished") >>> getText)
  )

parseTime :: String -> String -> Maybe UTCTime
parseTime f t = parseTimeM True defaultTimeLocale f t :: Maybe UTCTime

extractPublishedDates :: [C.ByteString] -> [String]
extractPublishedDates a = map datePublished $ mapMaybe decode a

parseDates :: [String] -> [UTCTime]
parseDates = mapMaybe (\d ->
                              parseTime "%Y-%m-%d"                 d  <|>
                              parseTime "%B %e, %Y"                d  <|>
                              parseTime "%Y-%m-%d %H:%M:%S"        d  <|>
                              parseTime "%Y-%m-%dT%H:%M:%S%Z"      d  <|>
                              parseTime "%B %k, %Y, %H:%M %p"      d  <|>
                              parseTime "%Y-%m-%d %H:%M:%S.000000" d
                        )

fromHtml :: String -> IO(Maybe UTCTime)
fromHtml html = do
  let doc = parseHtml html
  scripts   <- extractFromLdJson doc
  let ldJson = extractPublishedDates scripts
  heads     <- extractFromHead doc
  bodies    <- extractFromBody doc
  return $ listToMaybe $ parseDates $ heads ++ bodies ++ ldJson

fromUrl :: String -> IO(Maybe UTCTime)
fromUrl url = do
  body <- readUrl url
  fromHtml body

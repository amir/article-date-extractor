module Web.ArticleDateExtractor
  (
    fromUrl,
    fromHtml
  ) where

import Text.XML.HXT.Core
import Data.Char (toLower)
import Control.Applicative
import Data.Time (UTCTime)
import Network.Curl (curlGetString)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.HandsomeSoup hiding (fromUrl)
import Network.Curl.Opts (CurlOption (CurlFollowLocation))
import Data.Time.Format (parseTimeM, defaultTimeLocale)

readUrl :: String -> IO String
readUrl url = do
  (_, rsp) <- curlGetString url [CurlFollowLocation True]
  return rsp

hasAttrValue' :: ArrowXml a => String -> String -> a XmlTree XmlTree
hasAttrValue' a v = (hasAttrValue a (map toLower >>> (== v)))

extractFromHead doc = runX $ doc >>> css "meta" >>>
  (
    (hasAttrValue' "name"       "pubdate"                       ) <+>
    (hasAttrValue' "name"       "publishdate"                   ) <+>
    (hasAttrValue' "name"       "timestamp"                     ) <+>
    (hasAttrValue' "name"       "dc.date.issued"                ) <+>
    (hasAttrValue' "property"   "article:published_time"        ) <+>
    (hasAttrValue' "name"       "date"                          ) <+>
    (hasAttrValue' "property"   "bt:pubdate"                    ) <+>
    (hasAttrValue' "name"       "sailthru.date"                 ) <+>
    (hasAttrValue' "name"       "article.published"             ) <+>
    (hasAttrValue' "name"       "published-date"                ) <+>
    (hasAttrValue' "name"       "article.created"               ) <+>
    (hasAttrValue' "name"       "article_date_original"         ) <+>
    (hasAttrValue' "name"       "cxenseparse:recs:publishtime"  ) <+>
    (hasAttrValue' "name"       "date_published"                ) <+>
    (hasAttrValue' "itemprop"   "datepublished"                 ) <+>
    (hasAttrValue' "itemprop"   "datecreated"                   ) <+>
    (hasAttrValue' "name"       "created"                       ) <+>
    (hasAttrValue' "http-equiv" "date"                          )
  ) >>> getAttrValue "content"

extractFromBody doc = runX $ doc >>>
  (
    (css "time" >>> getAttrValue "datetime")                                    <+>
    (css "span" >>> hasAttrValue "itemprop" (== "datePublished") >>> getText)
  )

parseTime :: String -> String -> Maybe UTCTime
parseTime f t = (parseTimeM True) defaultTimeLocale f t :: Maybe UTCTime

parseDates :: [String] -> [UTCTime]
parseDates ds = mapMaybe (\d ->
                              (parseTime "%Y-%m-%d"             d)  <|>
                              (parseTime "%B %e, %Y"            d)  <|>
                              (parseTime "%Y-%m-%d %H:%M:%S"    d)  <|>
                              (parseTime "%Y-%m-%dT%H:%M:%S%Z"  d)  <|>
                              (parseTime "%B %k, %Y, %H:%M %p"  d)
                        ) ds

fromHtml :: String -> IO(Maybe UTCTime)
fromHtml html = do
  doc       <- return $ parseHtml html
  heads     <- extractFromHead doc
  bodies    <- extractFromBody doc
  return $ listToMaybe $ parseDates $ heads ++ bodies

fromUrl :: String -> IO(Maybe UTCTime)
fromUrl url = do
  body <- readUrl url
  date <- fromHtml body
  return date

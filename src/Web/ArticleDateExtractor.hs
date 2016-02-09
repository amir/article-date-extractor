module Web.ArticleDateExtractor
  (
    fromUrl,
    fromHtml
  ) where

import Text.XML.HXT.Core
import Control.Applicative
import Data.Time (LocalTime)
import Network.Curl (curlGetString)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.HandsomeSoup hiding (fromUrl)
import Network.Curl.Opts (CurlOption (CurlFollowLocation))
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)

readUrl :: String -> IO String
readUrl url = do
  (_, rsp) <- curlGetString url [CurlFollowLocation True]
  return rsp

extractFromHead doc = runX $ doc >>> css "meta" >>>
  (
    (hasAttrValue "name"        (== "pubdate"))                         <+>
    (hasAttrValue "name"        (== "publishdate"))                     <+>
    (hasAttrValue "name"        (== "timestamp"))                       <+>
    (hasAttrValue "name"        (== "dc.date.issued"))                  <+>
    (hasAttrValue "property"    (== "article:published_time"))          <+>
    (hasAttrValue "name"        (== "date"))                            <+>
    (hasAttrValue "property"    (== "bt:pubdate"))                      <+>
    (hasAttrValue "name"        (== "sailthru.date"))                   <+>
    (hasAttrValue "name"        (== "article.published"))               <+>
    (hasAttrValue "name"        (== "published-date"))                  <+>
    (hasAttrValue "name"        (== "article.created"))                 <+>
    (hasAttrValue "name"        (== "article_date_original"))           <+>
    (hasAttrValue "name"        (== "cxenseparse:recs:publishtime"))    <+>
    (hasAttrValue "name"        (== "date_published"))                  <+>
    (hasAttrValue "itemprop"    (== "datepublished"))                   <+>
    (hasAttrValue "itemprop"    (== "datecreated"))                     <+>
    (hasAttrValue "property"    (== "og:image"))                        <+>
    (hasAttrValue "itemprop"    (== "image"))                           <+>
    (hasAttrValue "http-equiv"  (== "date"))

  ) >>> getAttrValue "content"

extractFromBody doc = runX $ doc >>>
  (
    (css "time" >>> getAttrValue "datetime") <+>
    (css "span" >>> hasAttrValue "itemprop" (== "datePublished") >>> getText)
  )

parseTime :: String -> String -> Maybe LocalTime
parseTime f t = (parseTimeM True) defaultTimeLocale f t :: Maybe LocalTime

parseDates :: [String] -> [LocalTime]
parseDates ds = mapMaybe (\d ->
                              (parseTime "%Y-%m-%d %H:%M:%S" d) <|> (parseTime "%Y-%m-%dT%H:%M:%S%Z" d)
                        ) ds

fromHtml :: String -> IO(Maybe LocalTime)
fromHtml html = do
  doc       <- return $ parseHtml html
  heads     <- extractFromHead doc
  bodies    <- extractFromBody doc
  return $ listToMaybe $ parseDates $ heads ++ bodies

fromUrl :: String -> IO(Maybe LocalTime)
fromUrl url = do
  body <- readUrl url
  date <- fromHtml body
  return date

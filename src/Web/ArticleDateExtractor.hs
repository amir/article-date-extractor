module Web.ArticleDateExtractor where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time (LocalTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Text.XML.HXT.Core
import Text.HandsomeSoup hiding (fromUrl)
import Network.Curl (curlGetString)
import Network.Curl.Opts (CurlOption (CurlFollowLocation))

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

parseDates :: [String] -> [LocalTime]
parseDates t = mapMaybe (\x -> (parseTimeM True) defaultTimeLocale "%Y-%m-%d %H:%M:%S" x :: Maybe LocalTime) t

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

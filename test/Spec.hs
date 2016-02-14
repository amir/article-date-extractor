module Main where

import Data.Time
import Test.Hspec
import Web.ArticleDateExtractor

main :: IO ()
main = hspec $ do
  describe "fromHtml" $ do
    it "extracts datePublished from JSON-LD" $ do
      let h = "<script type=\"application/ld+json\">{\"datePublished\": \"2015-11-18 08:32:00.000000\"}</script>"
      date <- fromHtml h
      date `shouldBe` Just (UTCTime (fromGregorian 2015 11 18) (fromIntegral (30720 :: Integer)))

    it "extracts Nothing from invalid HTML" $ do
      date <- fromHtml "<a></b>"
      date `shouldBe` Nothing

    it "extracts date when attribute value is not lowercase" $ do
      date <- fromHtml "<meta name=\"DATE\" content=\"February 14, 2016\" />"
      date `shouldBe` Just (UTCTime (fromGregorian 2016 2 14) (fromIntegral (0 :: Integer)))

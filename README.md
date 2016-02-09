# article-date-extractor
Extracts an online article's publication date. Heavily influenced by [Webhose/article-date-extractor](https://github.com/Webhose/article-date-extractor).

## Examples

### Using `fromHtml`
```haskell
import Web.ArticleDateExtractor

main :: IO ()
main = do
  html <- readFile "/home/amir/index.html"
  date <- fromHtml html
  print date
  -- outputs: Just 2016-02-08 09:09:48 UTC
```

### Using `fromUrl`
```haskell
import Web.ArticleDateExtractor

main :: IO ()
main = do
  date <- fromUrl "http://edition.cnn.com/2015/11/28/opinions/sutter-cop21-paris-preview-two-degrees/index.html"
  print date
  -- outputs: Just 2015-11-29 00:44:59 UTC
```

### Installing
Not published to Hackage, yet. If you're using [stack](http://docs.haskellstack.org/en/stable/README.html), add a new location to your `stack.yaml`'s `packages` section:
```yaml
- location:
    git: git@github.com:amir/article-date-extractor.git
    commit: 99ef6be71de5f8b665c1c6ec04ef8b3e45691f38
```

And then add `article-date-extractor == 0.1.0.0` to `build-depends` section of your `cabal` file.

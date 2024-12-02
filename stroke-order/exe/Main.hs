module Main (main) where

import Control.Category ((>>>))
import Control.Monad (when)
import Data.Foldable (fold, forM_, toList)
import Data.Function ((&))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import DataLoading (WordDef (..), interleave, loadHskWords, loadTocflWords, shuffleGroups, uniqueBy)
import System.Directory (doesFileExist)
import System.IO (stderr)
import Unicode.Char.General.Scripts qualified as UC

maskHints :: WordDef -> WordDef
maskHints wd = wd {wdEng = T.map go (wdEng wd)}
  where
    go c = case UC.script c of
      UC.Han -> '？'
      _ -> c

findStrokeData :: T.Text -> IO (Maybe (T.Text, T.Text))
findStrokeData c = do
  let p = "stroke-order/data/hanzi-writer-data/data/" <> c <> ".json" & T.unpack
  doesFileExist p >>= \case
    False -> do
      "Can't find char " <> c & T.hPutStrLn stderr
      pure Nothing
    True -> do
      jsonTxt <- T.readFile p
      when (any (`T.elem` jsonTxt) ("\n\t" :: String)) $
        fail ("Json contains special: " <> p)
      Just (c, jsonTxt) & pure

main :: IO ()
main = do
  hskWords <- loadHskWords & fmap shuffleGroups
  tocflWords <- loadTocflWords & fmap shuffleGroups
  let ws = interleave (toList hskWords) (toList tocflWords) & fmap maskHints

  let charMap =
        ws
          & L.concatMap
            ( \wd ->
                wdTxt wd
                  & T.unpack
                  & uniqueBy id
                  & fmap (T.singleton >>> (,NE.singleton wd))
            )
          & M.fromListWith (<>)
          & fmap NE.reverse

  let uniqueChars =
        L.concatMap (wdTxt >>> T.unpack) ws
          & uniqueBy id
          & fmap T.singleton

  withStrokeData <- traverse findStrokeData uniqueChars & fmap catMaybes

  [ "Char",
    "Words",
    "Pinyin",
    "English",
    "IsTrad",
    "StrokeData"
    ]
    & L.intersperse "\t"
    & fold
    & T.putStrLn

  forM_ withStrokeData $ \(c, strokeData) -> do
    firstDef NE.:| allDefs <-
      charMap M.!? c
        & maybe (fail "Not found in charMap") pure

    let isTrad = wdTrad firstDef
    let defs = firstDef NE.:| (filter (wdTrad >>> (== isTrad)) allDefs & take 2)

    [ c,
      fmap wdTxt defs & commaSep,
      fmap wdPinyin defs & commaSep,
      fmap (wdEng >>> sanitize) defs & commaSep,
      if isTrad then "繁體" else "简体",
      strokeData
      ]
      & L.intersperse "\t"
      & fold
      & T.putStrLn
  where
    commaSep = NE.intersperse "，" >>> fold
    sanitize = T.map (\c -> if c == '，' then ',' else c)

module Main (main) where

import Control.Category ((>>>))
import Control.Monad (when, (>=>))
import Data.Bifunctor (second)
import Data.Foldable (fold, forM_, toList)
import Data.Function ((&))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)
import System.IO (stderr)
import System.Random (StdGen, mkStdGen, uniformR)
import Text.Read qualified as T
import Unicode.Char.General.Scripts qualified as UC

data WordDef = WordDef
  { wdTxt :: !T.Text,
    wdPinyin :: !T.Text,
    wdEng :: !T.Text,
    wdTrad :: !Bool
  }
  deriving (Show)

parseHskRow :: T.Text -> IO (Int, WordDef)
parseHskRow =
  T.splitOn "\t" >>> \case
    [lvlTxt, wdTxt, wdPinyin, _, wdEng]
      | Just lvl <- T.readMaybe (T.unpack lvlTxt) -> (lvl, WordDef {wdTrad = False, ..}) & pure
    cells -> "Could not parse" <> show cells & fail

stripQuotes :: T.Text -> IO T.Text
stripQuotes =
  (T.stripSuffix "\"" >=> T.stripPrefix "\"")
    >>> maybe (fail "Could not strip quotes") pure

parseTocflRow :: T.Text -> IO (Int, WordDef)
parseTocflRow =
  T.splitOn "\t"
    >>> traverse stripQuotes
    >=> \case
      [wdTxt, wdPinyin, _, lvlTxt, wdEng, _]
        | Just lvl <- T.readMaybe (T.unpack lvlTxt) -> (lvl, WordDef {wdTrad = True, ..}) & pure
      cells -> "Could not parse" <> show cells & fail

loadHskWords :: IO (Seq.Seq (Seq.Seq WordDef))
loadHskWords = do
  ls <-
    T.readFile "stroke-order/data/hsk-word-list.tsv"
      & fmap (T.lines >>> drop 1) -- skip headings
  lvls <-
    traverse parseHskRow ls
      & fmap (fmap (second Seq.singleton) >>> M.fromListWith (<>))
  M.elems lvls & Seq.fromList & pure

loadTocflWords :: IO (Seq.Seq (Seq.Seq WordDef))
loadTocflWords = do
  ls <-
    T.readFile "stroke-order/data/tocfl.tsv"
      & fmap (T.lines >>> drop 1) -- skip headings
  lvls <-
    traverse parseTocflRow ls
      & fmap (fmap (second Seq.singleton) >>> M.fromListWith (<>))
  M.elems lvls & Seq.fromList & pure

maskHints :: WordDef -> WordDef
maskHints wd = wd {wdEng = T.map go (wdEng wd)}
  where
    go c = case UC.script c of
      UC.Han -> '？'
      _ -> c

shuffleGroups :: forall a. Seq.Seq (Seq.Seq a) -> Seq.Seq a
shuffleGroups = fmap (shuffleGroup stdGen) >>> fold
  where
    stdGen = mkStdGen 0

    shuffleGroup :: StdGen -> Seq.Seq a -> Seq.Seq a
    shuffleGroup g' = \case
      Seq.Empty -> mempty
      s ->
        let (i, g'') = uniformR (0, Seq.length s & pred) g'
            x = s Seq.!? i & fromMaybe (error "OOB")
            s' = Seq.deleteAt i s
         in x Seq.<| shuffleGroup g'' s'

uniqueBy :: (Ord b) => (a -> b) -> [a] -> [a]
uniqueBy f = go mempty
  where
    go seen = \case
      x : xs | f x `S.notMember` seen -> x : go (S.insert (f x) seen) xs
      _ : xs -> go seen xs
      [] -> []

interleave :: [a] -> [a] -> [a]
interleave xs ys = case (xs, ys) of
  (x : xs', y : ys') -> x : y : interleave xs' ys'
  ([], _) -> ys
  (_, []) -> xs

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

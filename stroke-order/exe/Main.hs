module Main (main) where

import Control.Category ((>>>))
import Control.Monad (when, (>=>))
import Data.Bifunctor (second)
import Data.Foldable (fold, forM_, toList)
import Data.Function ((&))
import Data.List qualified as L
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

data WordDef = WordDef
  { wdTxt :: !T.Text,
    wdPinyin :: !T.Text,
    wdEng :: !T.Text,
    wdTrad :: !Bool
  }
  deriving (Show)

data CharInCtx = CharInCtx
  { ctxIdx :: Int,
    ctxDef :: WordDef
  }

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

allChars :: WordDef -> [CharInCtx]
allChars wd =
  T.unpack (wdTxt wd)
    & zip [0 ..]
    & fmap (fst >>> (`CharInCtx` wd))

charInCtxChar :: CharInCtx -> T.Text
charInCtxChar CharInCtx {..} =
  wdTxt ctxDef
    & T.unpack
    & (!! ctxIdx)
    & T.singleton

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

findStrokeData :: CharInCtx -> IO (Maybe (T.Text, CharInCtx))
findStrokeData ctx = do
  let c = charInCtxChar ctx
  let p = "stroke-order/data/hanzi-writer-data/data/" <> c <> ".json" & T.unpack
  doesFileExist p >>= \case
    False -> do
      "Can't find char " <> c & T.hPutStrLn stderr
      pure Nothing
    True -> do
      jsonTxt <- T.readFile p
      when (any (`T.elem` jsonTxt) ("\n\t" :: String)) $
        fail ("Json contains special: " <> p)
      Just (jsonTxt, ctx) & pure

main :: IO ()
main = do
  hskWords <- loadHskWords & fmap shuffleGroups
  tocflWords <- loadTocflWords & fmap shuffleGroups
  let ws = interleave (toList hskWords) (toList tocflWords)

  let uniques =
        toList ws
          & concatMap allChars
          & uniqueBy charInCtxChar
  withStrokeData <- traverse findStrokeData uniques & fmap catMaybes

  [ "Char",
    "Word",
    "Idx",
    "IsTrad",
    "Pinyin",
    "English",
    "StrokeData"
    ]
    & L.intersperse "\t"
    & fold
    & T.putStrLn

  forM_ withStrokeData $ \(strokeData, c@CharInCtx {ctxIdx, ctxDef = WordDef {..}}) ->
    [ charInCtxChar c,
      wdTxt,
      show ctxIdx & T.pack,
      show wdTrad & T.pack,
      wdPinyin,
      wdEng,
      strokeData
    ]
      & L.intersperse "\t"
      & fold
      & T.putStrLn

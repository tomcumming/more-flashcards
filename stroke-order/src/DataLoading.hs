module DataLoading
  ( WordDef (..),
    loadHskWords,
    loadTocflWords,
    shuffleGroup,
    shuffleGroups,
    uniqueBy,
    interleave,
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Bifunctor (second)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Random (StdGen, mkStdGen, uniformR)
import Text.Read qualified as T

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

shuffleGroup :: forall a. Seq.Seq a -> Seq.Seq a
shuffleGroup = go stdGen
  where
    stdGen = mkStdGen 0

    go :: StdGen -> Seq.Seq a -> Seq.Seq a
    go g' = \case
      Seq.Empty -> mempty
      s ->
        let (i, g'') = uniformR (0, Seq.length s & pred) g'
            x = s Seq.!? i & fromMaybe (error "OOB")
            s' = Seq.deleteAt i s
         in x Seq.<| go g'' s'

shuffleGroups :: Seq.Seq (Seq.Seq a) -> Seq.Seq a
shuffleGroups = fmap shuffleGroup >>> fold

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

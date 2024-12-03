module Main (main) where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Sequence qualified as Sq
import Data.Text qualified as T
import Data.Text.IO qualified as T
import DataLoading (WordDef (..), interleave, loadHskWords, loadTocflWords, shuffleGroups, uniqueBy, wdTxt)
import Pinyin qualified as Pinyin (normalise)

putRowLn :: [T.Text] -> IO ()
putRowLn = List.intersperse "\t" >>> fold >>> T.putStrLn

boolean :: Bool -> T.Text
boolean = bool "0" "1"

main :: IO ()
main = do
  hskWords <- loadHskWords & fmap shuffleGroups
  tocflWords <- loadTocflWords & fmap shuffleGroups

  let allWords =
        interleave (toList hskWords) (toList tocflWords)
          & zip [0 :: Int ..]
          & fmap (\(n, wd) -> (wdTxt wd, (n, NE.singleton wd)))
          & M.fromListWith
            (\(n, d) (m, e) -> (min n m, d <> e))
          & toList
          & List.sortOn fst
          & fmap snd
          & Sq.fromList

  ["Word", "Simp", "Trad", "Pinyin", "English"] & putRowLn

  forM_ allWords $ \wds -> do
    let word = NE.head wds & wdTxt
    let isSimp = any (wdTrad >>> not) wds & boolean
    let isTrad = any wdTrad wds & boolean
    let pinyin =
          wds
            & fmap
              (\WordDef {wdPinyin, wdTxt} -> Pinyin.normalise wdTxt wdPinyin)
            & toList
            & uniqueBy id
            & List.intersperse ", "
            & fold
    let english =
          wds
            & fmap wdEng
            & NE.sortBy (comparing T.length)
            & NE.intersperse "; "
            & fold

    [word, isSimp, isTrad, pinyin, english]
      & putRowLn

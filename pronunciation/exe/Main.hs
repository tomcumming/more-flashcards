module Main (main) where

import CCCEdict qualified
import CEdict qualified
import Control.Category ((>>>))
import Control.Monad (forM_, unless)
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import DataLoading (WordDef (..), interleave, loadHskWords, loadTocflWords, shuffleGroups, uniqueBy, wdTxt)
import Streaming.Prelude qualified as St
import System.IO (stderr)

putRowLn :: [T.Text] -> IO ()
putRowLn = List.intersperse "\t" >>> fold >>> T.putStrLn

wordAndChars :: T.Text -> [T.Text]
wordAndChars w = fmap T.singleton (T.unpack w) <> [w]

ensurePinyinLength :: T.Text -> Sq.Seq T.Text -> IO (Sq.Seq T.Text)
ensurePinyinLength txt ps
  | Sq.length ps == T.length txt = pure ps
  | otherwise = "Wrong pinyin length: " <> T.unpack txt & fail

pinyinFromDefs :: T.Text -> Sq.Seq CCCEdict.Definition -> IO (S.Set (Sq.Seq T.Text))
pinyinFromDefs w =
  traverse
    ( CCCEdict.defPinyin
        >>> toList
        >>> T.unwords
        >>> CEdict.parsePinyins
        >>> maybe
          ("Could not parse py for word: " <> T.unpack w & fail)
          ( fmap (\(txt, tn) -> txt <> T.pack (show tn))
              >>> ensurePinyinLength w
          )
    )
    >>> fmap (toList >>> S.fromList)

childCharacters :: T.Text -> S.Set T.Text
childCharacters w
  | T.length w > 1 = T.unpack w & fmap T.singleton & S.fromList
  | otherwise = mempty

loadDefinitionsForWordsAndChars ::
  [T.Text] ->
  IO (M.Map T.Text (Sq.Seq CCCEdict.Definition))
loadDefinitionsForWordsAndChars ws = do
  let css = S.fromList ws <> foldMap S.singleton ws
  St.readFile "stroke-order/data/cedict_1_0_ts_utf-8_mdbg.txt" $
    St.map T.pack
      >>> CCCEdict.parseLines
      >>> St.map
        ( \d ->
            M.fromList
              [ (CCCEdict.defSimp d, Sq.singleton d),
                (CCCEdict.defTrad d, Sq.singleton d)
              ]
              & M.filterWithKey (\w _ -> S.member w css)
        )
      >>> St.fold_ (M.unionWith (<>)) mempty id

showPinyins :: S.Set (Sq.Seq T.Text) -> Sq.Seq T.Text
showPinyins =
  toList
    >>> foldMap (toList >>> T.unwords >>> S.singleton)
    >>> S.toList
    >>> Sq.fromList

showEnglish :: (Foldable t) => t T.Text -> T.Text
showEnglish =
  toList
    >>> List.sortOn score
    >>> List.intersperse ", "
    >>> fold
  where
    score :: T.Text -> (Int, Int)
    score txt =
      ( [ T.isInfixOf "variant of" txt,
          T.isInfixOf "CL:" txt,
          T.isInfixOf "[" txt || T.isInfixOf "]" txt,
          T.isInfixOf ")" txt || T.isInfixOf "(" txt
        ]
          & filter id
          & length,
        T.length txt
      )

subsetPinyin :: S.Set T.Text -> S.Set T.Text -> Bool
subsetPinyin ws cs =
  S.isSubsetOf
    ws
    (foldMap (\p -> S.fromList [p, T.dropEnd 1 p <> "5"]) cs)

data Card = Card
  { crdTxt :: T.Text,
    crdPinyin :: Sq.Seq T.Text,
    crdEng :: Sq.Seq T.Text,
    crdParent :: Maybe T.Text
  }

makeCards ::
  M.Map T.Text (Sq.Seq CCCEdict.Definition) ->
  T.Text ->
  IO [Card]
makeCards dss w = case dss M.!? w of
  Nothing -> do
    "cedict missing: " <> w & T.hPutStrLn stderr
    pure []
  Just ds -> do
    wPys <- pinyinFromDefs w ds
    let wCard =
          Card
            { crdTxt = w,
              crdEng = foldMap CCCEdict.defEng ds,
              crdPinyin = showPinyins wPys,
              crdParent = Nothing
            }
    cs <-
      toList (childCharacters w)
        & fmap (\c -> (c, c))
        & M.fromList
        & traverse
          ( \c ->
              dss M.!? c
                & maybe (T.unpack c <> " missing" & fail) pure
          )

    -- Set of pinyins for each char in word def
    let wPyst =
          toList wPys
            & fmap toList
            & List.transpose
            & fmap S.fromList
            & zip (T.unpack w & fmap T.singleton)
            & M.fromListWith (<>)

    cPys <-
      M.traverseWithKey pinyinFromDefs cs
        <&> fmap (showPinyins >>> toList >>> S.fromList)

    let completelyCovered = M.intersectionWith subsetPinyin wPyst cPys & and
    T.hPutStrLn stderr ("Not covered by children: " <> w) & unless completelyCovered

    let cCards =
          M.foldMapWithKey
            ( \c py ->
                Card
                  { crdTxt = c,
                    crdEng = crdEng wCard,
                    crdPinyin = toList py & Sq.fromList,
                    crdParent = Just w
                  }
                  & pure @[]
            )
            cPys

    (cCards <> if completelyCovered then [] else [wCard]) & pure

main :: IO ()
main = do
  hskWords <- loadHskWords & fmap shuffleGroups
  tocflWords <- loadTocflWords & fmap shuffleGroups

  -- All the words we are learning
  let ws =
        interleave (toList hskWords) (toList tocflWords)
          & fmap wdTxt
          & concatMap wordAndChars
          & uniqueBy id
  ds <- loadDefinitionsForWordsAndChars ws

  cards <-
    traverse (makeCards ds) ws
      & fmap (concat >>> uniqueBy crdTxt)

  ["Word", "Pinyin", "Parent", "English"] & putRowLn
  forM_ cards $ \Card {..} ->
    [ crdTxt,
      toList crdPinyin & List.intersperse ", " & fold,
      fromMaybe "" crdParent,
      showEnglish crdEng
    ]
      & putRowLn

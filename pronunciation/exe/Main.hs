module Main (main) where

import CCCEdict qualified
import CEdict qualified
import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Foldable (find, fold, toList)
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
import System.Environment (getArgs)
import System.IO (stderr)

putRowLn :: [T.Text] -> IO ()
putRowLn = List.intersperse "\t" >>> fold >>> T.putStrLn

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

doEnglish :: IO ()
doEnglish = do
  hskWords <- loadHskWords & fmap shuffleGroups
  tocflWords <- loadTocflWords & fmap shuffleGroups

  let ws =
        interleave (toList hskWords) (toList tocflWords)
          & fmap wdTxt
          & uniqueBy id
  let wss = S.fromList ws

  dss <-
    St.readFile "stroke-order/data/cedict_1_0_ts_utf-8_mdbg.txt" $
      St.map T.pack
        >>> CCCEdict.parseLines
        >>> St.filter
          ( \CCCEdict.Definition {..} ->
              S.member defSimp wss || S.member defTrad wss
          )
        >>> St.map
          ( \d ->
              S.fromList [CCCEdict.defSimp d, CCCEdict.defTrad d]
                & toList
                & fmap (,Sq.singleton d)
                & M.fromListWith (<>)
          )
        >>> St.fold_ (M.unionWith (<>)) mempty id

  ["Word", "Pinyin", "English"] & putRowLn
  forM_ ws $ \w ->
    dss M.!? w & \case
      Nothing -> T.hPutStrLn stderr ("Not found: " <> w)
      Just ds -> do
        pys <-
          pinyinFromDefs w ds
            <&> ( toList
                    >>> fmap (toList >>> T.unwords)
                    >>> List.intersperse ", "
                    >>> fold
                )
        let eng = foldMap CCCEdict.defEng ds & showEnglish
        [w, pys, eng] & putRowLn

data PronunciationCard = PronunciationCard
  { pcClue :: T.Text,
    pcChar :: T.Text,
    pcPrimary :: (T.Text, T.Text, T.Text),
    pcSecondary :: Maybe (T.Text, T.Text, T.Text)
  }

doPronunciation :: IO ()
doPronunciation = do
  hskWords <- loadHskWords <&> shuffleGroups
  tocflWords <- loadTocflWords <&> shuffleGroups

  let charLearnOrder =
        interleave (toList hskWords) (toList tocflWords)
          & foldMap (wdTxt >>> charsInWord >>> toList)
          & List.nub

  let defIndex =
        interleave (toList hskWords) (toList tocflWords)
          & foldMap
            ( \wd ->
                charsInWord (wdTxt wd)
                  & toList
                  & fmap (,Sq.singleton wd)
            )
          & M.fromListWith (flip (<>))

  let cards =
        charLearnOrder
          <&> \c -> case lookupCharDefs defIndex c of
            (prim, maybeSec) ->
              PronunciationCard
                { pcClue =
                    wdTxt prim
                      & T.map (\c2 -> if c == T.singleton c2 then c2 else '_'),
                  pcChar = c,
                  pcPrimary = (wdTxt prim, wdPinyin prim, wdEng prim),
                  pcSecondary =
                    maybeSec
                      <&> \sec -> (wdTxt sec, wdPinyin sec, wdEng sec)
                }

  let cardCols = \case
        Just (txt, pinyin, eng) -> [txt, pinyin, eng]
        Nothing -> ["", "", ""]

  cards
    & fmap
      ( \PronunciationCard {..} ->
          pcChar : pcClue : cardCols (Just pcPrimary) <> cardCols pcSecondary
      )
    & fmap (T.intercalate "\t")
    & mapM_ T.putStrLn
  where
    charsInWord :: T.Text -> S.Set T.Text
    charsInWord = T.unpack >>> foldMap (T.singleton >>> S.singleton)

    lookupCharDefs :: M.Map T.Text (Sq.Seq WordDef) -> T.Text -> (WordDef, Maybe WordDef)
    lookupCharDefs defIndex c
      | wdTxt simplestDef == c,
        Just wordDef <- maybeWordDef =
          (wordDef, Just simplestDef)
      | wdTxt simplestDef == c = (simplestDef, Nothing)
      | otherwise = (simplestDef, maybeCharDef)
      where
        defs = defIndex M.! c
        simplestDef =
          defs Sq.!? 0
            & fromMaybe (error "Impossible: no definitions for character")
        maybeCharDef = find (wdTxt >>> (== c)) defs
        maybeWordDef = find (wdTxt >>> (/= c)) defs

main :: IO ()
main =
  getArgs >>= \case
    ["pronunciation"] -> doPronunciation
    ["translation"] -> doEnglish
    args ->
      "Use arg 'pronunciation' or 'translation': "
        <> show args
        & fail

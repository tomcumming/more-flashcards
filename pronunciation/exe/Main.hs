module Main (main) where

import CCCEdict qualified
import CEdict qualified
import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Bifunctor (second)
import Data.Foldable (fold, toList)
import Data.Foldable1 (maximumBy)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
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
    pcWord :: T.Text,
    pcPinyin :: T.Text,
    pcTranslation :: T.Text
  }

doPronunciation :: IO ()
doPronunciation = do
  hskWords <- loadHskWords
  tocflWords <- loadTocflWords

  let css =
        M.unionWith
          max
          (charScores hskWords)
          (charScores tocflWords)

  let charOrder =
        interleave (toList hskWords) (toList tocflWords)
          & fold
          & foldMap (wdTxt >>> charsInWord >>> toList)
          & List.nub

  let allWords =
        (hskWords <> tocflWords)
          & fold
          & foldMap (wdTxt >>> S.singleton)

  let wordIndex =
        allWords
          & foldMap
            ( \w ->
                charsInWord w
                  & foldMap ((,S.singleton w) >>> List.singleton)
            )
          & M.fromListWith (<>)

  let defIndex =
        tocflWords <> hskWords -- prefer hsk def
          & foldMap (foldMap (\wd -> [(wdTxt wd, wd)]))
          & M.fromList

  let coveringWords = go css wordIndex (S.fromList charOrder) charOrder

  let wordsWithDefs =
        orderCovering css coveringWords
          & fmap (\w -> (w, defIndex M.! w))

  let cards = makeWritingCards wordsWithDefs

  forM_ cards $ \PronunciationCard {..} ->
    [pcClue, pcWord, pcPinyin, pcTranslation]
      & T.intercalate "\t"
      & T.putStrLn
  where
    charsInWord :: T.Text -> S.Set T.Text
    charsInWord = T.unpack >>> foldMap (T.singleton >>> S.singleton)

    charScores :: Sq.Seq (Sq.Seq WordDef) -> M.Map T.Text Float
    charScores gs =
      zip [0 ..] (toList gs)
        & foldMap (\(lvl, ds) -> foldMap (wdTxt >>> charsInWord >>> toList >>> fmap (,lvl)) ds & toList)
        & fmap (second ((Sq.length gs -) >>> fromIntegral >>> (/ fromIntegral (Sq.length gs))))
        & M.fromListWith max

    go :: M.Map T.Text Float -> M.Map T.Text (S.Set T.Text) -> S.Set T.Text -> [T.Text] -> [T.Text]
    go css wordIndex remChars = \case
      [] -> []
      (c : cs)
        | S.member c remChars ->
            let lws = liveWords wordIndex c
                bestWord = maximumBy (comparing (score css remChars)) lws
             in bestWord
                  : go css wordIndex (S.difference remChars (charsInWord bestWord)) cs
      (_ : cs) -> go css wordIndex remChars cs

    score :: M.Map T.Text Float -> S.Set T.Text -> T.Text -> (Float, Float)
    score css remChars w =
      w
        & charsInWord
        & S.intersection remChars
        & foldMap ((css M.!) >>> Sum)
        & getSum
        & (,-(T.length w & fromIntegral))

    -- Should never be empty...
    liveWords :: M.Map T.Text (S.Set T.Text) -> T.Text -> NE.NonEmpty T.Text
    liveWords wi = (wi M.!?) >>> fromMaybe mempty >>> toList >>> NE.fromList

    orderCovering :: M.Map T.Text Float -> [T.Text] -> [T.Text]
    orderCovering css =
      List.sortOn (charsInWord >>> S.size)
        >>> List.sortOn (charsInWord >>> toList >>> fmap (css M.!) >>> minimum)
        >>> List.sortOn (charsInWord >>> toList >>> fmap (css M.!) >>> maximum)
        >>> List.reverse

    makeWritingCards :: [(T.Text, WordDef)] -> [PronunciationCard]
    makeWritingCards =
      foldMap (\(w, wd) -> charsInWord w & toList & fmap (,(w, wd)))
        >>> List.nubBy ((==) `on` fst)
        >>> fmap (uncurry goCard)
      where
        goCard :: T.Text -> (T.Text, WordDef) -> PronunciationCard
        goCard c (w, wd) =
          PronunciationCard
            { pcClue =
                T.map
                  (\c2 -> if T.singleton c2 == c then c2 else '_')
                  w,
              pcWord = w,
              pcPinyin = wdPinyin wd,
              pcTranslation =
                missingTranslations M.!? w
                  & fromMaybe (wdEng wd)
            }

    -- \| From google translate, not great...
    missingTranslations :: M.Map T.Text T.Text
    missingTranslations =
      M.fromList
        [ ("電視台", "PRC state TV network"),
          ("下午茶", "Afternoon tea"),
          ("還要", "Also"),
          ("如果說", "If one were to say"),
          ("曬太陽", "To be in the sun"),
          ("笑嘻嘻", "Grinning"),
          ("奇蹟", "Miracle"),
          ("轉帳", "Transfer"),
          ("橋樑", "Bridge"),
          ("鮮艶", "Fresh"),
          ("嘴脣", "Lips"),
          ("吸菸", "Smoking"),
          ("鈕扣", "Buttons"),
          ("潮溼", "Wet"),
          ("艱鉅", "Difficult"),
          -- 'Variant of...' definitions
          ("台灣", "Taiwan"),
          ("這裏", "Here"),
          ("澈底", "Totally"),
          ("倒楣", "Unlucky"),
          ("櫃臺", "Counter"),
          ("傢伙", "Guys"),
          ("砲", "Cannon")
        ]

main :: IO ()
main =
  getArgs >>= \case
    ["pronunciation"] -> doPronunciation
    ["translation"] -> doEnglish
    args ->
      "Use arg 'pronunciation' or 'translation': "
        <> show args
        & fail

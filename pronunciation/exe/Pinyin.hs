module Pinyin (normalise) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Data.Bifunctor (bimap, first, second)
import Data.Char (isAlpha)
import Data.Foldable (Foldable (fold), toList)
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Sequence qualified as Sq
import Data.Text qualified as T

type Tone = Int

accents :: M.Map Char (Char, Tone)
accents =
  M.fromList
    [ ('ā', ('a', 1)),
      ('á', ('a', 2)),
      ('ǎ', ('a', 3)),
      ('à', ('a', 4)),
      ('a', ('a', 5)),
      ('ē', ('e', 1)),
      ('é', ('e', 2)),
      ('ě', ('e', 3)),
      ('è', ('e', 4)),
      ('e', ('e', 5)),
      ('ī', ('i', 1)),
      ('í', ('i', 2)),
      ('ǐ', ('i', 3)),
      ('ì', ('i', 4)),
      ('i', ('i', 5)),
      ('ō', ('o', 1)),
      ('ó', ('o', 2)),
      ('ǒ', ('o', 3)),
      ('ò', ('o', 4)),
      ('o', ('o', 5)),
      ('ū', ('u', 1)),
      ('ú', ('u', 2)),
      ('ǔ', ('u', 3)),
      ('ù', ('u', 4)),
      ('u', ('u', 5)),
      ('ǖ', ('v', 1)),
      ('ǘ', ('v', 2)),
      ('ǚ', ('v', 3)),
      ('ǜ', ('v', 4)),
      ('ü', ('v', 5))
    ]

initials :: Sq.Seq T.Text
initials =
  Sq.fromList
    [ "b",
      "c",
      "ch",
      "d",
      "f",
      "g",
      "h",
      "j",
      "k",
      "l",
      "m",
      "n",
      "p",
      "q",
      "r",
      "s",
      "sh",
      "t",
      "w",
      "x",
      "y",
      "z",
      "zh"
    ]
    & Sq.sortOn (T.length >>> Down)

finals :: Sq.Seq T.Text
finals =
  Sq.fromList
    [ "a",
      "ai",
      "an",
      "ang",
      "ao",
      "e",
      "ei",
      "en",
      "eng",
      "er",
      "eng",
      "i",
      "ie",
      "ia",
      "iao",
      "ian",
      "iang",
      "iong",
      "in",
      "iu",
      "ing",
      "o",
      "ou",
      "on",
      "ong",
      "u",
      "uan",
      "uang",
      "ua",
      "ui",
      "uai",
      "ue",
      "uo",
      "un",
      "v",
      "ve"
    ]
    & Sq.sortOn (T.length >>> Down)

parseInitial :: T.Text -> [(T.Text, T.Text)]
parseInitial inTxt = do
  i <- toList initials <> [""]
  txt <- T.stripPrefix i inTxt & maybeToList
  pure (i, txt)

parseFinal :: T.Text -> [((T.Text, Tone), T.Text)]
parseFinal = \inTxt -> do
  f <- toList finals
  go inTxt f
    & maybeToList
    & fmap (first (second (fromMaybe 5)))
  where
    go :: T.Text -> T.Text -> Maybe ((T.Text, Maybe Tone), T.Text)
    go inTxt f = case (T.uncons inTxt, T.uncons f) of
      (_, Nothing) -> pure (("", Nothing), inTxt)
      (Just (c1, inTxt'), Just (c2, f')) | Just (c, t) <- matchChar c1 c2 -> do
        go inTxt' f' & fmap (first (bimap (T.singleton c <>) (t <|>)))
      _ -> Nothing

matchChar :: Char -> Char -> Maybe (Char, Maybe Tone)
matchChar c1 c2
  | c1 == c2 = Just (c1, Nothing)
  | Just (c3, t) <- accents M.!? c1, c3 == c2 = Just (c3, Just t)
  | otherwise = Nothing

parsePinyin :: T.Text -> [((T.Text, Tone), T.Text)]
parsePinyin inTxt = do
  (i, inTxt2) <- parseInitial inTxt -- or nothing
  ((f, t), inTxt3) <- parseFinal inTxt2
  pure ((i <> f, t), inTxt3)

parsePinyins :: T.Text -> [[T.Text]]
parsePinyins = \case
  "" -> [[]]
  "diǎnr" -> [["dian3", "r"]]
  "ng" -> [["ng"]]
  "tóur" -> [["tou2", "r"]]
  inTxt -> do
    ((p, t), inTxt') <- parsePinyin inTxt
    fmap ((p <> showTone t) :) (parsePinyins inTxt')

showTone :: Tone -> T.Text
showTone = \case
  5 -> ""
  t -> show t & T.pack

normalise :: T.Text -> T.Text -> T.Text
normalise wdTxt inTxt =
  T.filter isAlpha inTxt
    & T.map nonStandards
    & T.toLower
    & parsePinyins
    & filter (length >>> wordCount wdTxt)
    & listToMaybe
    & (fromMaybe ([wdTxt, inTxt] & T.unwords & T.unpack & error))
    & L.intersperse " "
    & fold

wordCount :: T.Text -> Int -> Bool
wordCount wdTxt c = case wdTxt of
  "窗" | c == 2 -> True
  "越" | c == 2 -> True
  "角色" | c == 1 -> True
  "秘" | c == 2 -> True
  "嘗" | c == 2 -> True
  "汙" | c == 2 -> True
  "不至" | c == 3 -> True
  "迴" | c == 2 -> True
  "小伙" | c == 3 -> True
  "剎" | c == 2 -> True
  _ -> T.length wdTxt == c

nonStandards :: Char -> Char
nonStandards = \case
  'ɑ' -> 'a'
  c -> c

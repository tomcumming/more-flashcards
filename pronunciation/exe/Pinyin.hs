module Pinyin (normalise) where

import Control.Category ((>>>))
import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Sequence qualified as Sq
import Data.Text qualified as T

type Tone = Int

initials :: Sq.Seq T.Text
initials =
  Sq.fromList
    [ "b",
      "p",
      "m",
      "f",
      "d",
      "t",
      "n",
      "l",
      "g",
      "k",
      "h",
      "j",
      "q",
      "x",
      "zh",
      "ch",
      "sh",
      "r",
      "z",
      "c",
      "s",
      "y",
      "w"
    ]
    & Sq.sortOn (T.length >>> Down)

vowels :: Sq.Seq T.Text
vowels = Sq.fromList ["a", "e", "i", "o", "u", "ü"]
    & Sq.sortOn (T.length >>> Down)

finals :: Sq.Seq T.Text
finals = Sq.fromList ["n", "ng", "r"]
    & Sq.sortOn (T.length >>> Down)

mids :: Sq.Seq (T.Text, (T.Text, Tone))
mids =
  Sq.fromList
    [ ("ā", ("a", 1)),
      ("á", ("a", 2)),
      ("ǎ", ("a", 3)),
      ("à", ("a", 4)),
      ("a", ("a", 5)),
      ("ē", ("e", 1)),
      ("é", ("e", 2)),
      ("ě", ("e", 3)),
      ("è", ("e", 4)),
      ("e", ("e", 5)),
      ("ī", ("i", 1)),
      ("í", ("i", 2)),
      ("ǐ", ("i", 3)),
      ("ì", ("i", 4)),
      ("i", ("i", 5)),
      ("ō", ("o", 1)),
      ("ó", ("o", 2)),
      ("ǒ", ("o", 3)),
      ("ò", ("o", 4)),
      ("o", ("o", 5)),
      ("ū", ("u", 1)),
      ("ú", ("u", 2)),
      ("ǔ", ("u", 3)),
      ("ù", ("u", 4)),
      ("u", ("u", 5)),
      ("ǖ", ("v", 1)),
      ("ǘ", ("v", 2)),
      ("ǚ", ("v", 3)),
      ("ǜ", ("v", 4)),
      ("ü", ("v", 5))
    ]

parseOneOf :: Sq.Seq T.Text -> T.Text -> [(T.Text, T.Text)]
parseOneOf ss inTxt =
  ss
    & toList
    & mapMaybe (\i -> T.stripPrefix i inTxt & fmap (i,))

parsePinyin :: T.Text -> [(T.Text, T.Text)]
parsePinyin inTxt = do
  (i, inTxt2) <- parseOneOf initials inTxt <> [("", inTxt)]
  (m1, inTxt3) <- parseOneOf vowels inTxt2 <> [("", inTxt2)]
  ((m2, t), inTxt4) <- parseMid inTxt3
  (m3, inTxt5) <- parseOneOf vowels inTxt4 <> [("", inTxt4)]
  (f, inTxt6) <- parseOneOf finals inTxt5 <> [("", inTxt5)]

  let normd = i <> m1 <> m2 <> m3 <> f

  -- unfortunates
  if
    | normd == "tour" -> pure ()
    | T.isSuffixOf "ur" normd -> fail "ur"
    | otherwise -> pure ()

  pure (normd <> toneNum t, inTxt6)

parseMid :: T.Text -> [((T.Text, Int), T.Text)]
parseMid inTxt = do
  (s, (s', t)) <- toList mids
  inTxt2 <- T.stripPrefix s inTxt & maybeToList
  pure ((s', t), inTxt2)

parsePinyins :: T.Text -> [[T.Text]]
parsePinyins inTxt = do
  (p, inTxt') <- parsePinyin inTxt
  if T.null inTxt'
    then pure [p]
    else parsePinyins inTxt' & fmap (p :)

toneNum :: Tone -> T.Text
toneNum = \case
  5 -> ""
  t | t >= 1 && t <= 4 -> show t & T.pack
  t -> "Unknown tone: " <> show t & error

normalise :: T.Text -> T.Text -> T.Text
normalise wdTxt inTxt
  | wdTxt == "越" = "yue4"
  | wdTxt == "差點兒" = "cha4dian3r"
  | wdTxt == "有點兒" = "you3dian3r"
  | wdTxt == "嗯" = "ng"
  | otherwise =
      T.filter isAlpha inTxt
        & T.map nonStandards
        & T.toLower
        & parsePinyins
        & filter (length >>> (== count))
        & \case
          ps : _ -> T.unwords ps
          pss -> ["Failed to parse pinyin:"
            , T.unpack wdTxt
            , T.unpack inTxt
            , show pss
            ] & unwords & error
  where
    count
      | wdTxt == "窗" = 2
      | wdTxt == "秘" = 2
      | wdTxt == "嘗" = 2
      | wdTxt == "汙" = 2
      | wdTxt == "迴" = 2
      | wdTxt == "小伙" = 3
      | wdTxt == "不至" = 3
      | wdTxt == "剎" = 2
      | otherwise = T.length wdTxt

nonStandards :: Char -> Char
nonStandards = \case
  'ɑ' -> 'a'
  c -> c

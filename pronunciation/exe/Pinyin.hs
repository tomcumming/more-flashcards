module Pinyin
  ( normalise,
    initials,
    accents,
    Tone,
    finals,
    unAccent,
  )
where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Char (isAlpha)
import Data.Foldable (Foldable (fold), toList)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Text qualified as T

type Tone = Int

accents :: M.Map Char (Char, Tone)
accents =
  M.fromList
    [ ('ā', ('a', 1)),
      ('á', ('a', 2)),
      ('ǎ', ('a', 3)),
      ('à', ('a', 4)),
      ('ē', ('e', 1)),
      ('é', ('e', 2)),
      ('ě', ('e', 3)),
      ('è', ('e', 4)),
      ('ī', ('i', 1)),
      ('í', ('i', 2)),
      ('ǐ', ('i', 3)),
      ('ì', ('i', 4)),
      ('ō', ('o', 1)),
      ('ó', ('o', 2)),
      ('ǒ', ('o', 3)),
      ('ò', ('o', 4)),
      ('ū', ('u', 1)),
      ('ú', ('u', 2)),
      ('ǔ', ('u', 3)),
      ('ù', ('u', 4)),
      ('ǖ', ('v', 1)),
      ('ǘ', ('v', 2)),
      ('ǚ', ('v', 3)),
      ('ǜ', ('v', 4))
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

unUmlaut :: Char -> Char
unUmlaut = \case
  'ü' -> 'v'
  c -> c

unAccent :: T.Text -> (T.Text, Tone)
unAccent txt =
  ( T.map (\c -> accents M.!? c & maybe c fst) txt & T.map unUmlaut,
    T.unpack txt
      & mapMaybe (accents M.!?)
      & listToMaybe
      & maybe 5 snd
  )

showTone :: Tone -> T.Text
showTone = \case
  5 -> ""
  t -> show t & T.pack

parsePinyin ::
  M.Map Char (S.Set T.Text) ->
  T.Text ->
  T.Text ->
  [((T.Text, Tone), T.Text, T.Text)]
parsePinyin uh wdTxt inTxt = case T.uncons wdTxt of
  Just (c, wdTxt') | Just ms <- uh M.!? c -> do
    pres <- toList ms
    let (py, tone) = T.take (T.length pres) inTxt & unAccent
    py == pres & guard
    pure ((py, tone), wdTxt', T.drop (T.length pres) inTxt)
  _ -> []

corrections :: M.Map (T.Text, T.Text) [(T.Text, Tone)]
corrections =
  M.fromList
    [ (("姊姊", "jiějie"), [("zi", 3), ("zi", 3)]),
      (("窗", "chuānghu"), [("chuang", 1)]),
      (("越", "yuèyuè"), [("yue", 4)]),
      (("角色", "jiǎo"), [("jiao", 3), ("se", 4)]),
      (("垃圾", "lèsè"), [("le", 4), ("se", 4)]),
      (("姊妹", "jiěmèi"), [("zi", 3), ("mei", 4)]),
      (("秘", "mìmì"), [("mi", 4)]),
      (("嘗", "chángshì"), [("chang", 2)]),
      (("暫時", "zhànshí"), [("zan", 5), ("shi", 2)]),
      (("概括", "gàiguā"), [("gai", 4), ("kuo", 4)]),
      (("艘", "sāo"), [("sou", 1)]),
      (("哎喲", "āiyāo"), [("ai", 1), ("you", 1)]),
      (("汙", "wūrǎn"), [("wu", 1)]),
      (("混淆", "hùnyáo"), [("hun", 4), ("xiao", 2)]),
      (("忽略", "hūluè"), [("hu", 1), ("lve", 4)]),
      (("略微", "luèwēi"), [("lve", 4), ("wei", 1)]),
      (("骯髒", "āngzhāng"), [("ang", 1), ("zang", 1)]),
      (("恐吓", "kǒnghè"), [("king", 3), ("he", 4)]),
      (("战略", "zhànluè"), [("zhan", 4), ("lve", 4)]),
      (("說服", "shuìfú"), [("shuo", 1), ("fu", 2)]),
      (("囊括", "nángguā"), [("nang", 2), ("kuo", 4)]),
      (("懸崖", "xuányái"), [("xuan", 2), ("ya", 2)]),
      (("掠夺", "luèduó"), [("lve", 4), ("duo", 2)]),
      (("曝光", "bàoguāng"), [("bao", 4), ("guang", 1)]),
      (("供給", "gōnjǐ"), [("gong", 1), ("ji", 3)]),
      (("不至", "bùzhìyú"), [("bu", 4), ("zhi", 4)]),
      (("迴", "huíxiǎng"), [("hui", 2)]),
      (("蝸牛", "guāniú"), [("wo", 1), ("niu", 2)]),
      (("小伙", "xiǎohuǒzi"), [("xiao", 3), ("huo", 3)]),
      (("步驟", "bùzòu"), [("bu", 4), ("zhou", 4)]),
      (("短暫", "duǎnzhàn"), [("duan", 3), ("zan", 4)]),
      (("賜", "sì"), [("si", 4)]),
      (("剎", "shāchē"), [("sha", 1)])
    ]

parsePinyins ::
  M.Map Char (S.Set T.Text) ->
  T.Text ->
  T.Text ->
  [[(T.Text, Tone)]]
parsePinyins uh wdTxt inTxt
  | T.null wdTxt && T.null inTxt = [[]]
  | Just cs <- corrections M.!? (wdTxt, inTxt) = [cs]
  | otherwise = do
      ((p, t), wdTxt', inTxt') <- parsePinyin uh wdTxt inTxt
      ps <- parsePinyins uh wdTxt' inTxt'
      (p, t) : ps & pure

normalise :: M.Map Char (S.Set T.Text) -> T.Text -> T.Text -> T.Text
normalise uh wdTxt inTxt =
  parsePinyins uh wdTxt (cleanUpPinyin inTxt)
    & errorEmpty
    & fmap (fmap showPair >>> List.intersperse " " >>> fold)
    & List.intersperse ", "
    & fold
  where
    showPair :: (T.Text, Tone) -> T.Text
    showPair (txt, tone) = txt <> showTone tone

    errorEmpty :: [[(T.Text, Tone)]] -> [[(T.Text, Tone)]]
    errorEmpty rs
      | null rs = T.unwords ["emptyError", wdTxt, cleanUpPinyin inTxt] & T.unpack & error
      | otherwise = rs

cleanUpPinyin :: T.Text -> T.Text
cleanUpPinyin = T.filter isAlpha >>> T.map nonStandards >>> T.toLower

nonStandards :: Char -> Char
nonStandards = \case
  'ɑ' -> 'a'
  c -> c

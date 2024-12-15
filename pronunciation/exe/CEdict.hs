module CEdict (parsePinyins) where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Text qualified as T
import Pinyin qualified

initials :: S.Set T.Text
initials = S.fromList $ toList Pinyin.initials

finals :: S.Set T.Text
finals = S.fromList $ toList Pinyin.finals

isValid :: T.Text -> Bool
isValid inTxt
  | S.member inTxt initials = True
  | S.member inTxt finals = True
  | otherwise = or $ do
      f <- mapMaybe (`T.stripPrefix` inTxt) (toList initials)
      pure $ S.member f finals

normalise :: T.Text -> T.Text
normalise =
  T.toLower
    >>> T.replace "u:" "v"
    >>> T.replace "hng" "heng" -- 哼 hng5

parsePinyin ::
  T.Text ->
  Maybe (T.Text, Pinyin.Tone)
parsePinyin inTxt = do
  (pyTxt', toneTxt) <- T.unsnoc inTxt
  let pyTxt = normalise pyTxt'
  tone <- case toneTxt of
    '1' -> pure 1
    '2' -> pure 2
    '3' -> pure 3
    '4' -> pure 4
    '5' -> pure 5
    _ -> Nothing
  guard $ isValid pyTxt
  pure (pyTxt, tone)

parsePinyins ::
  T.Text ->
  Maybe (Sq.Seq (T.Text, Pinyin.Tone))
parsePinyins =
  T.splitOn " "
    >>> filter (/= "-")
    >>> traverse parsePinyin
    >>> fmap Sq.fromList

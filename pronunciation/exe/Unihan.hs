module Unihan (loadUnihan) where

import Control.Category ((>>>))
import Control.Monad (forM, guard)
import Data.Char qualified as Char
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Numeric (readHex)
import Pinyin (finals, initials, unAccent)
import Streaming.Prelude qualified as St

parseSinglePinyin :: T.Text -> IO (S.Set T.Text)
parseSinglePinyin = \case
  "ň" -> pure (S.singleton "n")
  "ǹ" -> pure (S.singleton "n")
  "ń" -> pure (S.singleton "n")
  "n" -> pure (S.singleton "n")
  "ḿ" -> pure (S.singleton "m")
  "m̀" -> pure (S.singleton "m")
  "m̄" -> pure (S.singleton "m")
  "ế" -> pure (S.singleton "e")
  "ê̌" -> pure (S.singleton "e")
  "ề" -> pure (S.singleton "e")
  "ê̄" -> pure (S.singleton "e")
  "ǹg" -> pure (S.singleton "ng")
  "ňg" -> pure (S.singleton "ng")
  "ńg" -> pure (S.singleton "ng")
  "ng" -> pure (S.singleton "ng")
  "hm" -> pure (S.singleton "hm")
  "hng" -> pure (S.singleton "hng")
  "r" -> pure (S.singleton "r")
  txt -> maybe (T.unpack txt & fail) pure $ listToMaybe $ do
    initial <- toList initials <> [""]
    txt2 <- T.stripPrefix initial txt & maybe [] pure
    let (final, _) = unAccent txt2
    elem final finals & guard
    initial <> final & S.singleton & pure

parseKHanyu :: T.Text -> IO [T.Text]
parseKHanyu = T.splitOn " " >>> traverse go >>> fmap concat
  where
    go =
      T.splitOn ":" >>> \case
        [_, pys] -> T.splitOn "," pys & pure
        khy -> fail (T.unpack (List.intersperse ":" khy & fold))

parsePinlu :: T.Text -> IO [T.Text]
parsePinlu = T.splitOn " " >>> traverse go >>> fmap concat
  where
    go :: T.Text -> IO [T.Text]
    go =
      T.splitOn "(" >>> \case
        [py, _] -> pure [py]
        txt -> fail (T.unpack (List.intersperse "(" txt & fold))

parseUnihanLine :: String -> IO (Maybe (Char, S.Set T.Text))
parseUnihanLine =
  T.pack >>> \line -> case T.splitOn "\t" line of
    _ | T.isPrefixOf "#" line -> pure Nothing
    [ccStr, "kMandarin", pys] -> do
      c <- parseCC (T.unpack ccStr)
      ps <- forM (T.splitOn " " pys) $ \py -> parseSinglePinyin py
      Just (c, fold ps) & pure
    [ccStr, "kHanyuPinyin", pys] -> do
      c <- parseCC (T.unpack ccStr)
      pyss <- parseKHanyu pys
      ps <- forM pyss $ \py -> parseSinglePinyin py
      Just (c, fold ps) & pure
    [ccStr, "kHanyuPinlu", pys] -> do
      c <- parseCC (T.unpack ccStr)
      pyss <- parsePinlu pys
      ps <- forM pyss $ \py -> parseSinglePinyin py
      Just (c, fold ps) & pure
    [_, _, _] -> pure Nothing
    _ -> fail ("parseUnihanLine " <> T.unpack line)
  where
    parseCC :: String -> IO Char
    parseCC = \case
      'U' : '+' : x ->
        readHex x
          & List.find (snd >>> null)
          & maybe (fail ("U+" <> x)) (fst >>> Char.chr >>> pure)
      x -> fail ("parseCC:" <> x)

loadUnihan :: IO (M.Map Char (S.Set T.Text))
loadUnihan =
  St.readFile "stroke-order/data/Unihan/Unihan_Readings.txt" $
    St.filter (null >>> not)
      >>> St.mapM parseUnihanLine
      >>> flip St.for St.each
      >>> St.fold_ (\p (c, ss) -> M.insertWith (<>) c ss p) mempty id

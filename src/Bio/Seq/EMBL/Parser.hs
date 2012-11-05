{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Parser for EMBL format
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : ftp://ftp.ebi.ac.uk/pub/databases/embl/release/usrman.txt
-- 
--
-----------------------------------------------------------------------------

module Bio.Seq.EMBL.Parser

       where

import           Bio.Seq.EMBL.Parser.Internal
import           Bio.Seq.EMBL.Types
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.Bits
import qualified Data.ByteString.Char8 as B8
import           Data.Char hiding (isSpace,isDigit)
import           Data.Int
import           Data.Maybe
import           Data.Word
import           Prelude hiding (takeWhile)


parseOranism = do
  (name,cName) <- parseOS
  cs <- parseOC
  maybeXX
  og <- optional parseOG
  return $ Oranism name cName cs og
  

parseRef :: Parser Reference
parseRef = do
  rn <- fmap RefNo parseRN
  rc <- optional $ fmap RefComment parseRC
  rp <- optional $ parseRP
  rx <- optional $ parseRX
  rg <- optional $ fmap RefGroup $ parseRG
  as <- fmap (map Author) parseRA
  title <- fmap Title parseRT
  rl <- parseRL
  return $ Reference rn rc rp rx rg as title rl
  
parseSQ = do
  _ <- mkHeader "SQ" *> "Sequence " .*> decimal <*. " BP;"
  aNum <- char ' ' *> decimal <*. " A;"
  cNum <- char ' ' *> decimal <*. " C;"
  gNum <- char ' ' *> decimal <*. " G;"
  tNum <- char ' ' *> decimal <*. " T;"
  oNum <- char ' ' *> decimal <*. " other;" <* endOfLine
  sdata <- fmap (B8.concat . concat) $
           many1 $
           count 5 (char ' ') *>
           (takeWhile1 (isIUPAC . fromIntegral . ord) `sepBy1` char ' ') <*
           skipSpace <* many1 digit <* endOfLine
  lineTM
  return (SQ (SeqSta aNum cNum gNum tNum oNum) sdata) <?> "Sequence Data"

-- | A very fast predicate for the official IUPAC-IUB single-letter base codes.
-- @
--    isIUPAC w == toEnum w `elem` "ABCDGHKMNRSTVWYabcdghkmnrstvwy"
-- @
isIUPAC :: Word8 -> Bool
isIUPAC w | pre <= 25 = unsafeShiftL (1 :: Int32)
                         (fromIntegral pre) .&. iupacMask /= 0
          | otherwise = False
  where
    pre = (w .|. 32) - 97 -- toLower w - ord 'a'
    iupacMask :: Int32
    iupacMask = 23999695
    -- iupacMask = foldr1 (.|.) $
    --             map (unsafeShiftL 1 . (\n -> n - 97) . ord . toLower)
    --             "ABCDGHKMNRSTVWY"
{-# INLINE isIUPAC #-}


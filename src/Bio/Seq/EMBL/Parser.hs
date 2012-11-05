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

parseID = do
  _ <- mkHeader "ID" 
  t1 <- takeWhile1 (/= ';') <* char ';' <?> "Primary accession number" 
  skipSpace
  t2 <- "SV" .*> skipSpace *>
        (decimal <?> "Sequence version number") <* char ';' 
  t3 <- skipSpace *>
        ((string "linear" <|> string "circular") <?> "Topology") <* char ';'
  t4 <- skipSpace *> takeWhile1 (/= ';') <*
        char ';' <?> "Molecule type"
  t5 <- skipSpace *>
        (string "CON" <|> -- Entry constructed from segment entry sequences;
                          -- if unannotated, annotation may be drawn from
                          -- segment entries
         string "PAT" <|> -- Patent
         string "EST" <|> -- Expressed Sequence Tag
         string "GSS" <|> -- Genome Survey Sequence
         string "HTC" <|> -- High Thoughput CDNA sequencing
         string "HTG" <|> -- High Thoughput Genome sequencing
         string "MGA" <|> -- Mass Genome Annotation
         string "WGS" <|> -- Whole Genome Shotgun
         string "TSA" <|> -- Transcriptome Shotgun Assembly
         string "STS" <|> -- Sequence Tagged Site
         string "STD" <?> -- Standard (all entries not classified as above)
         "Data Class"
        ) <* char ';'
  t6 <- skipSpace *>
        (string "PHG" <|> -- Bacteriophage
         string "ENV" <|> -- Environmental Sample    
         string "FUN" <|> -- Fungal      
         string "HUM" <|> -- Human
         string "INV" <|> -- Invertebrate             
         string "MAM" <|> -- Other Mammal            
         string "VRT" <|> -- Other Vertebrate         
         string "MUS" <|> -- Mus musculus
         string "PLN" <|> -- Plant
         string "PRO" <|> -- Prokaryote
         string "ROD" <|> -- Other Rodent
         string "SYN" <|> -- Synthetic
         string "TGN" <|> -- Transgenic
         string "UNC" <|> -- Unclassified
         string "VRL" <?> -- Viral
         "Taxonomic Division"
        ) <* char ';'
  t7 <- skipSpace *> (decimal <?> "Sequence Length") <*
        skipSpace <*. "BP." <* endOfLine
  undefined
  


                
parseDT = do
  mkHeader "DT" *>
    takeWhile1 (/= ' ') <* skipSpace

lineDT1 = do
  str1 <- fmap B8.unpack parseDT
  relNum <- "(Rel." .*> skipSpace *> decimal <* char ',' <*
            skipSpace <*. "Created)" <* endOfLine
  case toTime str1 of
    Nothing -> fail "Not a valid time string"
    Just t -> return (t,relNum)
  
lineDT2 = do
  str2 <- fmap B8.unpack parseDT
  relNum <- "(Rel." .*> skipSpace *> decimal <* char ',' <*
            skipSpace <*. "Last updated,"
  verNum <- skipSpace *> "Version" .*> skipSpace *> decimal <*
            char ')' <* endOfLine
  case toTime str2 of
    Nothing -> fail "Not a valid time string"
    Just t -> return (t,relNum,verNum)

parseDE = do
  mkHeader "DE" *> 
    takeWhile1 (/= '\n') <* endOfLine

  
parseKW = do
  fmap concat (goKW `sepBy1` char '\n') <* char '.' <* endOfLine
  where
    goKW = do
      mkHeader "KW" *>
        (takeWhile1 (\c -> c /= ';' && c /= '.' && c /= '\n') `sepBy`
         string "; " <?> "Keywords sepBy \"; \"")
        <* (option "" $ string ";")

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


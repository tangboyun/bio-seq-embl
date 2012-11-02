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
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char hiding (isSpace,isDigit)
import           Data.Int
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           Data.Word
import           Prelude hiding (takeWhile)
import           System.Locale

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

parseOS = do
  _ <- mkHeader "OS"
  des <- fmap (B8.intercalate " ") $
         takeWhile1 (\c -> c /= '(' && c /= '\n') `sepBy1`
         (endOfLine <* mkHeader "OS")
  name <- option Nothing $
          -- can handle "OS   Trifolium repens\nOS   (white\nOS   clover)\n"
          fmap (Just . B8.intercalate " ") $
          ((endOfLine *> mkHeader "OS" *> char '(' *>
            (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
             (endOfLine <* mkHeader "OS")) <* char ')') <|>
           (char '(' *> (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
                         (endOfLine <* mkHeader "OS")) <* char ')'))
  endOfLine
  return (des,name)

parseOC = do
  fmap concat (goOC `sepBy1` char '\n') <* char '.' <* endOfLine  
  where
    goOC = do
      mkHeader "OC" *>
        (takeWhile1 (\c -> c /= ';' && c /= '.' && c /= '\n') `sepBy`
         string "; " <?> "Organism Classification sepBy \"; \"") <*
        (option "" $ string ";")
        
parseOG = do
  mkHeader "OG" *>
    takeWhile1 (/= '\n') <* endOfLine

parseRN = do
  mkHeader "ON" *> char '[' *>
    decimal <* char ']' <* endOfLine 

parseRC = do
  fmap (B8.intercalate " ") $
    (mkHeader "RC" *> takeWhile1 (/= '\n') `sepBy1` endOfLine) <* endOfLine
  where
    

parseRP =
  goRP `sepBy1` endOfLine <* endOfLine
  where
    goRP = do
      mkHeader "RP" *>
        (do
            beg <- decimal
            _ <- char '-'
            end <- decimal
            return (beg,end)
        ) `sepBy1` string ", "


parseRX = do
  mkHeader "RX" *> parseResource <* endOfLine 
  where
    parseResource = parsePUBMED <|>
                    parseDOI <|>
                    parseAGRICOLA <?> "reference cross-reference"
      where
        parsePUBMED = fmap PUBMED
                      ("PUBMED" .*> string "; " *>
                       (takeWhile1 isDigit <?> "Invalid PMID") <*
                       char '.')
        parseDOI = fmap (DOI . B8.init)
                   ("DOI" .*> string "; " *>
                    (takeWhile1 (/= '\n')))
        parseAGRICOLA = fmap (AGRICOLA . B8.init)
                        ("AGRICOLA" .*> string "; " *>
                         (takeWhile1 (/= '\n')))
    
parseRG = do
  mkHeader "RG" *> takeWhile1 (/= '\n') <* endOfLine


parseRA = do
  fmap concat $
    goRA `sepBy1` string ",\n" <* char ';' <* endOfLine
  where
    goRA = do
      mkHeader "RA" *>
        takeWhile1
        (\c -> c /= ',' && c /= '\n' && c /= ';') `sepBy1`
        string ", "

    
parseRT = do
  fmap (trim . B8.intercalate " ") $ -- trim for rare case "RT   \"Title\nRT   \";\n"
    (
      mkHeader "RT" *> char '"' *>
      (fmap (:[]) $ takeWhile1 $ \c -> c /= '"' && c /= '\n' && c /= ';') <*
      char '"' <|>  -- one line title 
      (do
          line1 <- mkHeader "RT" *> char '"' *> takeWhile1 (/= '\n') <*
                   endOfLine <* mkHeader "RT"
          ls <- takeWhile (\c -> c /= '\n' && c /= '"') `sepBy1`
                (endOfLine *> mkHeader "RT")
          _ <- char '"'
          return $ line1 : ls) <|> -- multi-line title
      mkHeader "RT" *> return []  -- empty title
    ) <* char ';' <* endOfLine
       

parseRL :: Parser RefLoc
parseRL = parsePaper <|>
          parseSubmitted <|>
          parsePatent <|>
          parseUnpublished <|>
          parseMisc <|>
          parseBook <|>
          parseThesis <?> "Reference Location"
  
parseSQ = do
  slen <- mkHeader "SQ" *> "Sequence " .*> decimal <*. " BP;"
  aNum <- char ' ' *> decimal <*. " A;"
  cNum <- char ' ' *> decimal <*. " C;"
  gNum <- char ' ' *> decimal <*. " G;"
  tNum <- char ' ' *> decimal <*. " T;"
  oNum <- char ' ' *> decimal <*. " other;" <* endOfLine
  sdata <- fmap (SeqData . B8.concat . concat) $
           many1 $
           count 5 (char ' ') *>
           (takeWhile1 (isIUPAC . fromIntegral . ord) `sepBy1` char ' ') <*
           skipSpace <* many1 digit <* endOfLine
  lineTM
  return (SeqSta slen aNum cNum gNum tNum oNum,sdata) <?> "Sequence Data"

-- | A very fast predicate for the official IUPAC-IUB single-letter base codes.
-- @
--    isIUPAC w == toEnum w `elem` "ABCDGHKMNRSTVWYabcdghkmnrstvwy"
-- @
--     Code      Base Description
--     ----      --------------------------------------------------------------
--     G         Guanine
--     A         Adenine
--     T         Thymine
--     C         Cytosine
--     R         Purine               (A or G)
--     Y         Pyrimidine           (C or T or U)
--     M         Amino                (A or C)
--     K         Ketone               (G or T)
--     S         Strong interaction   (C or G)
--     W         Weak interaction     (A or T)
--     H         Not-G                (A or C or T) H follows G in the alphabet
--     B         Not-A                (C or G or T) B follows A
--     V         Not-T (not-U)        (A or C or G) V follows U
--     D         Not-C                (A or G or T) D follows C
--     N         Any                  (A or C or G or T)
--                                      A-1
isIUPAC :: Word8 -> Bool
isIUPAC w | pred <= 25 = unsafeShiftL (1 :: Int32)
                         (fromIntegral pred) .&. iupacMask /= 0
          | otherwise = False
  where
    pred = (w .|. 32) - 97 -- toLower w - ord 'a'
    iupacMask :: Int32
    iupacMask = 23999695
    -- iupacMask = foldr1 (.|.) $
    --             map (unsafeShiftL 1 . (\n -> n - 97) . ord . toLower)
    --             "ABCDGHKMNRSTVWY"
{-# INLINE isIUPAC #-}


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

lineID = do
  _ <- "ID" .*> count 3 (satisfy (== ' ')) <?> "ID Line"
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
  
lineAC = do
  _ <- "AC" .*> count 3 (satisfy (== ' ')) <?> "AC Line"
  (takeWhile1 (/= ';') <* char ';') `sepBy1` (many1 $ char ' ') <* endOfLine

linePR = do
  _ <- "PR" .*> count 3 (satisfy (== ' ')) <?> "PR Line"
  "Project:" .*> (decimal <?> "Project Number") <* char ';' <* endOfLine

toTime :: String -> Maybe UTCTime
toTime = parseTime defaultTimeLocale "%d-%b-%Y"

trim :: ByteString -> ByteString
trim str = if isSpace $ B8.last str
           then trim $ B8.init str
           else str
                
parseDT = do
  _ <- "DT" .*> count 3 (satisfy (== ' ')) <?> "DT Line"
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

lineDE = do
  _ <- "DE" .*> count 3 (satisfy (== ' ')) <?> "DE Line"
  takeWhile1 (/= '\n') <* endOfLine

lineKW = do
  _ <- "KW" .*> count 3 (satisfy (== ' ')) <?> "KW Line"
  ss <- takeWhile1 (\c -> c /= ';' && c /= '.' && c /= '\n') `sepBy`
        string "; " <?> "Keywords sepBy \"; \""
  _ <- option "" $ string ";"
  return ss
  
parseKW = do
  fmap concat (lineKW `sepBy1` char '\n') <* char '.' <* endOfLine

parseOS = do
  _ <- "OS" .*> count 3 (satisfy (== ' ')) <?> "OS Line"
  des <- fmap (B8.intercalate " ") $
         takeWhile1 (\c -> c /= '(' && c /= '\n') `sepBy1`
         (endOfLine <*. "OS   ")
  name <- option Nothing $
          -- can handle "OS   Trifolium repens\nOS   (white\nOS   clover)\n"
          fmap (Just . B8.intercalate " ") $
          ((endOfLine *> "OS   (" .*>
            (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
             (endOfLine <*. "OS   ")) <* char ')') <|>
           (char '(' *> (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
                         (endOfLine <*. "OS   ")) <* char ')'))
  endOfLine
  return (des,name)

lineOC = do
  _ <- "OC" .*> count 3 (satisfy (== ' ')) <?> "OC Line"
  ss <- takeWhile1 (\c -> c /= ';' && c /= '.' && c /= '\n') `sepBy`
        string "; " <?> "Organism Classification sepBy \"; \""
  _ <- option "" $ string ";"
  return ss

parseOC = do
  fmap concat (lineOC `sepBy1` char '\n') <* char '.' <* endOfLine  

lineOG = do
  "OG" .*> count 3 (satisfy (== ' ')) *>
    takeWhile1 (/= '\n') <*
    endOfLine <?> "OG Line"

lineRN = do
  "ON" .*> count 3 (satisfy (== ' ')) *> char '[' *>
    decimal <*
    char ']' <* endOfLine <?> "RN Line"

lineRC = do
  "RC" .*> count 3 (satisfy (== ' ')) *>
    takeWhile1 (/= '\n') <?> "RC Line"

parseRC = do
  comments <- lineRC `sepBy1` endOfLine
  endOfLine
  return $! B8.intercalate " " comments

lineRP = do
  "RP" .*> count 3 (satisfy (== ' ')) *>
    (do
        beg <- decimal
        _ <- char '-'
        end <- decimal
        return (beg,end)
    ) `sepBy1` string ", "

parseRP =
  lineRP `sepBy1` endOfLine <* endOfLine

parseResource = parsePUBMED <|>
                parseDOI <|>
                parseAGRICOLA
  where
    parsePUBMED = fmap PUBMED
                  (string "PUBMED" *> string "; " *>
                   (takeWhile1 isDigit <?> "Invalid PMID") <*
                   char '.')
    parseDOI = fmap (DOI . B8.init)
               (string "DOI" *> string "; " *>
                (takeWhile1 (/= '\n')))
    parseAGRICOLA = fmap (AGRICOLA . B8.init)
                    (string "AGRICOLA" *> string "; " *>
                     (takeWhile1 (/= '\n')))

lineRX = do
  ("RX" .*> count 3 (satisfy (== ' ')) *>
   parseResource <?> "RX Line") <* endOfLine 
  
lineRG = do
  ("RG" .*> count 3 (satisfy (== ' ')) <?> "RG Line") *>
    takeWhile1 (/= '\n') <* endOfLine

lineRA = do
  ("RA" .*> count 3 (satisfy (== ' ')) <?> "RA Line") *>
    (takeWhile1 (\c -> c /= ',' && c /= '\n' && c /= ';') `sepBy1` string ", ")

parseRA = do
  fmap concat $ (lineRA `sepBy1` string ",\n") <* char ';' <* endOfLine

lineRT = do
  "RT" .*> count 3 (satisfy (== ' ')) <?> "RT Line"
    
parseRT = do
  fmap (trim . B8.intercalate " ") $ -- trim for rare case "RT   \"Title\nRT   \";\n"
    (
      lineRT *> char '"' *>
      (fmap (:[]) $ takeWhile1 $ \c -> c /= '"' && c /= '\n' && c /= ';')
      <* char '"' <|>  -- one line title 
      (do
          line1 <- lineRT *> char '"' *> takeWhile1 (/= '\n') <* endOfLine <* lineRT
          ls <- takeWhile (\c -> c /= '\n' && c /= '"') `sepBy1` (endOfLine *> lineRT)
          _ <- char '"'
          return $ line1 : ls) <|> -- multi-line title
      lineRT *> return []  -- empty title
    ) <* char ';' <* endOfLine
       

parseRL :: Parser RefLoc
parseRL = parsePaper <|>
          parseSubmitted <|>
          parsePatent <|>
          parseUnpublished <|>
          parseMisc <|>
          parseBook <|>
          parseThesis

-- | A very fast predicate for the official IUPAC-IUB single-letter base codes.
-- @
--    isIUPAC w == w `elem` "ABCDGHKMNRSTVWYabcdghkmnrstvwy"
-- @
isIUPAC :: Word8 -> Bool
isIUPAC w | pred <= 25 = unsafeShiftL (1 :: Int32) (fromIntegral pred) .&. iupacMask /= 0
          | otherwise = False
{-# INLINE isIUPAC #-}
  where
    pred = (w .|. 32) - 97 -- toLower w - ord 'a'
    iupacMask :: Int32
    iupacMask = 23999695
    -- iupacMask = foldr1 (.|.) $
    --             map (unsafeShiftL 1 . (\n -> n - 97) . ord . toLower) "ABCDGHKMNRSTVWY"


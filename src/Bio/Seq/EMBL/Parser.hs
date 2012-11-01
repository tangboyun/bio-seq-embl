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

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           System.Locale
import Bio.Seq.EMBL.Types
import Prelude hiding (takeWhile)

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
parseRL = do
  parsePaper <|>
  parseSubmitted <|>
  parsePatent <|>
  parseUnpublished <|>
  parseMisc <|>
  parseBook <|>
  parseThesis
  where
    lineRL = do
      "RL" .*> count 3 (satisfy (== ' ')) <?> "RL Line"
    parsePaper = do
      (pub,vol,iss,pBeg,pEnd,y) <- parseJ
      return $ Paper pub vol iss (pBeg,pEnd) y
    parseMisc = do
      (pub,vol,iss,pBeg,pEnd,y) <- lineRL *> "(misc) " .*> parseJ
                                   return $ Misc pub vol iss (pBeg,pEnd) y
  
    parseJ = do
      pub <- fmap (Publication . B8.intercalate " ") $
             word `sepBy1` char ' '
      vol <- char ' ' *> decimal
      iss <- option Nothing (fmap Just $ char '(' *> decimal <* char ')')
             pBeg <- char ':' *> decimal <* char '-'
      pEnd <- decimal
      y    <- char '(' *> decimal <* char ')' <* char '.' <* endOfLine
              return (pub,vol,iss,pBeg,pEnd,y)
      where 
        word = takeWhile1
               (\c -> isAlpha_ascii c || isDigit c || c == '.') <*
               (do
                   c1 <- peekChar
                   case c1 of
                     Just ' ' -> return ()
                     _ -> fail "Not word boundary")
  
    parseBook = do
      auths <- fmap (map Author . concat) $
               lineRL *> "(in) " .*>
               (as `sepBy1` (char ',' *> endOfLine *> lineRL)) <*
               char ';' <* endOfLine
      bookName <- fmap (Publication . B8.intercalate " ") $
                  lineRL *> (takeWhile (\c -> c /= ':' && c /= '\n') `sepBy1`
                           (endOfLine <* lineRL)) <* char ':'
      pBeg <- decimal <* char '-'
      pEnd <- decimal <* char ';' <* endOfLine
      puber <- fmap (Publisher . trim . B8.intercalate " ") $
               lineRL *>
               takeWhile (\c -> c /= '(' && c /= '\n') `sepBy1`
               (endOfLine <* lineRL)
      y <- char '(' *> decimal <* char ')' <* char '.' <* endOfLine
      return $ Book bookName auths puber (pBeg,pEnd) y
      where
        as = takeWhile (\c -> c /= ',' && c /= ';') `sepBy1` string ", "
    
      parseSubmitted = do
        timeStr <- fmap B8.unpack $
                   lineRL *> "Submitted " .*> char '(' *>
                   takeWhile1 (/= ')') <* char ')'
        db <- fmap Database $ " to the " .*>
              takeWhile1 (/= ' ') <*. " database" <*
              option "" (string "s") <* char '.' <* endOfLine
        addr <- option Nothing $ fmap (Just . Address . B8.intercalate " ") $
                (lineRL *> takeWhile1 (/= '\n')) `sepBy1` endOfLine
        case toTime timeStr of
          Nothing -> fail "Not a valid time string"
          Just t -> return $ Submitted t db addr

      parseThesis = do
        y <- lineRL *> "Thesis " .*>
             char '(' *> decimal <* char ')' <*. ", "
        sch <- fmap (Address . B8.intercalate " ") $
               takeWhile1 (/= '\n') `sepBy1` (endOfLine <* lineRL)
        endOfLine
        return $ Thesis sch y

      parseUnpublished = do
        lineRL *> "Unpublished" .*>
          char '.' *> endOfLine *> return Unpublished

      parsePatent = do
        patNum <- fmap ApplicationNumber $
                  lineRL *>
                  "Patent number " .*> takeWhile1 (/= '-') <* char '-'
        patType <- fmap PatentType $ takeWhile1 (/= '/') <* char '/'
        sn <- decimal <*. ", "
        tStr <- fmap B8.unpack $
                takeWhile1 (/= '.') <* char '.' <* endOfLine
        appCan <- fmap (Applicant . B8.intercalate " ") $
                  (lineRL *> takeWhile (/= '\n')) `sepBy1` endOfLine
        endOfLine
        case toTime tStr of
          Nothing -> fail "Not a valid time string"
          Just t -> return $ Patent patNum patType sn t appCan


-- [65,66,67,68,71,72,75,77,78,82,83,84,86,87,89]
isIUPAC :: Word8 -> Bool
isIUPAC w | w > 121 = False
          | w > 89 = isIUPAC (w - 32)
          | w < 65 = False        
          | otherwise =
            case w of
              

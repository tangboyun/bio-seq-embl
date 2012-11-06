{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Low-level Parser, Ugly & Unstable
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module Bio.Seq.EMBL.Parser.Internal

       where

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
import           Data.Word
import           Prelude hiding (takeWhile)
import           System.Locale

toTime :: String -> Maybe UTCTime
toTime = parseTime defaultTimeLocale "%d-%b-%Y"

trim :: ByteString -> ByteString
trim "" = ""
trim str = if (== ' ') $ B8.last str
           then trim $ B8.init str
           else str

mkHeader :: ByteString -> Parser String
mkHeader str = str .*> count 3 (satisfy (== ' ')) <?>
               (B8.unpack str ++ " Line")

maybeXX = option "" (fmap B8.concat $ many1 lineXX) *> return ()

lineXX = "XX" .*> takeWhile (/= '\n') <* endOfLine
lineTM = "//" .*> return () <?> "termination line"

parseDC = "CON" .*> return CON <|> 
          "PAT" .*> return PAT <|> 
          "EST" .*> return EST <|> 
          "GSS" .*> return GSS <|> 
          "HTC" .*> return HTC <|> 
          "HTG" .*> return HTG <|> 
          "MGA" .*> return MGA <|> 
          "WGS" .*> return WGS <|> 
          "TSA" .*> return TSA <|> 
          "STS" .*> return STS <|> 
          "STD" .*> return STD <?> 
          "Data Class"

parseTAX = "PHG" .*> return PHG <|>
           "ENV" .*> return ENV <|>
           "FUN" .*> return FUN <|>
           "HUM" .*> return HUM <|>
           "INV" .*> return INV <|>
           "MAM" .*> return MAM <|>
           "VRT" .*> return VRT <|>
           "MUS" .*> return MUS <|>
           "PLN" .*> return PLN <|>
           "PRO" .*> return PRO <|>
           "ROD" .*> return ROD <|>
           "SYN" .*> return SYN <|>
           "TGN" .*> return TGN <|>
           "UNC" .*> return UNC <|>
           "VRL" .*> return VRL <?>
           "Taxonomic Division"

parseTOPO = "linear" .*> return Linear <|>
            "circular" .*> return Circular <?>
            "Topology"

parseID = do
  _ <- mkHeader "ID" 
  acc <- takeWhile1 (/= ';') <* char ';' <?> "Primary accession number" 
  skipSpace
  sv <- "SV" .*> skipSpace *>
        (decimal <?> "Sequence version number" :: Parser Int) <* char ';' 
  topo <- skipSpace *> parseTOPO <* char ';'
  mol <- skipSpace *> takeWhile1 (/= ';') <*
        char ';' <?> "Molecule type"
  dat <- skipSpace *> parseDC <* char ';'
  tax <- skipSpace *> parseTAX <* char ';'
  len <- skipSpace *> (decimal :: Parser Int) <*
         skipSpace <*. "BP." <* endOfLine
  return (acc,sv,topo,mol,dat,tax,len)
  
parseDT = do
  mkHeader "DT" *>
    takeWhile1 (/= ' ') <* skipSpace

lineDT1 = do
  str1 <- fmap B8.unpack parseDT
  relNum <- fmap Release $
            "(Rel." .*> skipSpace *> decimal <* char ',' <*
            skipSpace <*. "Created)" <* endOfLine
  case toTime str1 of
    Nothing -> fail "Not a valid time string"
    Just t -> return (t,relNum)
  
lineDT2 = do
  str2 <- fmap B8.unpack parseDT
  relNum <- fmap Release $
            "(Rel." .*> skipSpace *> decimal <* char ',' <*
            skipSpace <*. "Last updated,"
  verNum <- fmap Version $
            skipSpace *> "Version" .*> skipSpace *> decimal <*
            char ')' <* endOfLine
  case toTime str2 of
    Nothing -> fail "Not a valid time string"
    Just t -> return (t,relNum,verNum)

parseDE = do
  fmap (B8.intercalate " ") $
    mkHeader "DE" *> 
    takeWhile1 (/= '\n') `sepBy1`
    (endOfLine *> mkHeader "DE") <* endOfLine

  
parseKW = do
  fmap concat $ mkHeader "KW" *>
    (goKW `sepBy1` string "; ") `sepBy1`
    (char ';' *> endOfLine *> mkHeader "KW" ) <* char '.' <* endOfLine
  where
    goKW = fmap Keyword $ takeWhile (\c -> c /= ';' && c /= '.')

parseOS = do
  _ <- mkHeader "OS"
  des <- fmap (B8.intercalate " ") $
         (fmap trim $ takeWhile1 (\c -> c /= '(' && c /= '\n')) `sepBy1`
         (endOfLine <* mkHeader "OS")
  name <- optional $
          -- can handle "OS   Trifolium repens\nOS   (white\nOS   clover)\n"
          fmap (B8.intercalate " ") $
          ((endOfLine *> mkHeader "OS" *> char '(' *>
            (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
             (endOfLine <* mkHeader "OS")) <* char ')') <|>
           (char '(' *> (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
                         (endOfLine <* mkHeader "OS")) <* char ')'))
  endOfLine
  return (des,name)

parseOC = do
  fmap concat $
    mkHeader "OC" *>
    (takeWhile1 (\c -> c /= '.' && c /= ';') `sepBy1`
     string "; ") `sepBy1`
    (char ';' *> endOfLine *> mkHeader "OC") <*
    char '.' <* endOfLine
        
parseOG = do
  mkHeader "OG" *>
    takeWhile1 (/= '\n') <* endOfLine
  
parsePR = do
  mkHeader "PR" *> takeWhile (/= ';') <* char ';' <* endOfLine
  

parseAC = do
  fmap concat $ mkHeader "AC" *>
    (takeWhile1 (/= ';') `sepBy1`
     string "; ") `sepBy1` ( char ';' *> endOfLine *> mkHeader "AC") <*
    char ';' <* endOfLine
  
parsePaper = do
  (pub,vol,iss,pBeg,pEnd,y) <- mkHeader "RL" *> parseJ
  return $ Paper pub vol iss (pBeg,pEnd) y

parseMisc = do
  (pub,vol,iss,pBeg,pEnd,y) <- mkHeader "RL" *> "(misc) " .*> parseJ
  return $ Misc pub vol iss (pBeg,pEnd) y
  
parseJ = do
  pub <- fmap (Publication . B8.intercalate " ") $
         word `sepBy1` char ' '
  vol <- char ' ' *> decimal
  iss <- optional $
         char '(' *> decimal <* char ')'
  pBeg <- char ':' *> decimal <* char '-'
  pEnd <- decimal
  y    <- char '(' *> decimal <* char ')' <*
          char '.' <* endOfLine
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
           mkHeader "RL" *> "(in) " .*>
           (as `sepBy1` (char ',' *> endOfLine *> mkHeader "RL")) <*
           char ';' <* endOfLine
  bookName <- fmap (Publication . B8.intercalate " ") $
              mkHeader "RL" *>
              (takeWhile (\c -> c /= ':' && c /= '\n') `sepBy1`
               (endOfLine <* mkHeader "RL")) <* char ':'
  pBeg <- decimal <* char '-'
  pEnd <- decimal <* char ';' <* endOfLine
  puber <- fmap (Publisher . trim . B8.intercalate " ") $
           mkHeader "RL" *>
           takeWhile (\c -> c /= '(' && c /= '\n') `sepBy1`
           (endOfLine <* mkHeader "RL")
  y <- char '(' *> decimal <* char ')' <* char '.' <* endOfLine
  return $ Book bookName auths puber (pBeg,pEnd) y
  where
    as = takeWhile (\c -> c /= ',' && c /= ';') `sepBy1` string ", "
    
parseSubmitted = do
  timeStr <- fmap B8.unpack $
             mkHeader "RL" *> "Submitted " .*> char '(' *>
             takeWhile1 (/= ')') <* char ')'
  db <- fmap Database $ " to the " .*>
        takeWhile1 (/= ' ') <*. " database" <*
        option "" (string "s") <* char '.' <* endOfLine
  addr <- optional $
          fmap (Address . B8.intercalate " ") $
          (mkHeader "RL" *> takeWhile1 (/= '\n')) `sepBy1` endOfLine
  endOfLine
  case toTime timeStr of
    Nothing -> fail "Not a valid time string"
    Just t -> return $ Submitted t db addr

parseThesis = do
  y <- mkHeader "RL" *> "Thesis " .*>
       char '(' *> decimal <* char ')' <*. ", "
  sch <- fmap (Address . B8.intercalate " ") $
         takeWhile1 (/= '\n') `sepBy1` (endOfLine <* mkHeader "RL")
  endOfLine
  return $ Thesis sch y

parseUnpublished = do
  mkHeader "RL" *> "Unpublished" .*>
    char '.' *> endOfLine *> return Unpublished

parsePatent = do
  patNum <- fmap PatentNumber $
            mkHeader "RL" *>
            "Patent number " .*> takeWhile1 (/= '-') <* char '-'
  patType <- fmap PatentType $ takeWhile1 (/= '/') <* char '/'
  sn <- decimal <*. ", "
  tStr <- fmap B8.unpack $
          takeWhile1 (/= '.') <* char '.' <* endOfLine
  appCan <- fmap (Applicant . B8.intercalate " ") $
            (mkHeader "RL" *> takeWhile (/= '\n')) `sepBy1` endOfLine
  endOfLine
  case toTime tStr of
    Nothing -> fail "Not a valid time string"
    Just t -> return $ Patent patNum patType sn t appCan
    
parseRN :: Parser Int
parseRN = do
  mkHeader "RN" *> char '[' *>
    decimal <* char ']' <* endOfLine 

parseRC = do
  fmap (B8.intercalate " ") $
    (mkHeader "RC" *> takeWhile1 (/= '\n') `sepBy1` endOfLine) <* endOfLine

parseRP :: Parser [(Int,Int)]
parseRP =
  fmap concat $ (goRP `sepBy1` endOfLine) <* endOfLine
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


parseRT = empLine <|>
          oneLine <|>
          mulLine <?> "Reference Title"
  where
    empLine = mkHeader "RT" *> char ';' *> endOfLine *> return ""
    oneLine = mkHeader "RT" *> char '"' *>
              takeWhile1 (\c -> c /= '\n' && c /= '"') <*
              char '"' <* char ';' <* endOfLine
    mulLine = fmap (trim . B8.intercalate " ") $
              mkHeader "RT" *> char '"' *>
              (takeWhile (\c -> c /= '"' && c /= '\n') `sepBy1`
               (endOfLine *> mkHeader "RT")) <*
              char '"' <* char ';' <* endOfLine
              
    
parseRL :: Parser RefLoc
parseRL = parsePaper <|>
          parseSubmitted <|>
          parsePatent <|>
          parseUnpublished <|>
          parseMisc <|>
          parseBook <|>
          parseThesis <?> "Reference Location"


-- | Legal Char in Feature key and qualifier
-- Uppercase letters (A-Z) [65..90]
-- Lowercase letters (a-z) [97..122]
-- Numbers (0-9) [48..57]
-- Underscore (_) [95]
-- Hyphen (-)  [45]
-- Single quotation mark or apostrophe (') [39]
-- Asterisk (*) [42]
isLegalFKChar :: Word8 -> Bool
isLegalFKChar w | 97 <= w, w <= 122 = True
                | 65 <= w, w <= 90 = True
                | 48 <= w, w <= 57 = True
                | w == 95 || w == 39 ||
                  w == 42 || w == 45 = True
                | otherwise = False
{-# INLINE isLegalFKChar #-}          

parseCC = do
  fmap (B8.intercalate " ") $
    mkHeader "CC" *> takeWhile1 (/= '\n') `sepBy1`
    (endOfLine *> mkHeader "CC") <* endOfLine
  
parseFT :: Parser [Feature]
parseFT = do
  many1 lineFH *>
    many1 ( do
               (k,l) <- parseKey
               qvs <- option [] $ many1 parseQV
               return $ Feature k l qvs
          ) <?> "Feature Table"
  where
    legalName = takeWhile1 (isLegalFKChar . fromIntegral . ord)
    conFTLine = 
      mkHeader "FT" *> count 16 (satisfy (== ' ')) *> peekChar >>=
      \c ->
      case c of
        Just '/' -> fail "Not Location info"
        _ -> return ()

    parseKey = do
      key <- fmap Key $ mkHeader "FT" *> legalName
      loc <- fmap (Location . B8.intercalate " ") $ skipSpace *>
             (takeWhile1 (/= '\n') `sepBy1` (endOfLine *> conFTLine)) <*
             endOfLine
      return (key,loc)

    parseQV = do
      q <- fmap Qualifier $
           mkHeader "FT" *> count 16 (satisfy (== ' ')) *>
           char '/' *> legalName
      v <- fmap (Value . B8.intercalate " ") $
           char '=' *> (takeWhile1 (/= '\n') `sepBy1`
                        (endOfLine *> conFTLine)) <*
           endOfLine
      return (q,v)

    lineFH = "FH" .*> takeWhile (/= '\n') *> endOfLine

                                      
parseDR = mkHeader "DR" *> takeWhile (/= '.') <* char '.' <* endOfLine

parseASI = do
  lineAH *>
    many1 (do
              ls1 <- mkHeader "AS" *> decimal <* char '-'
              ls2 <- decimal <* skipSpace
              pI <- takeWhile1 (/= ' ') <* skipSpace
              ps <- ("not_available" .*> return Nothing) <|>
                    (do
                        p1 <- decimal <* char '-'
                        p2 <- decimal 
                        return $ Just (p1,p2)
                    )
              isC <- takeWhile (== ' ') *>
                     (do
                         c <- peekChar
                         case c of
                           Just 'c' -> char 'c' *>
                                       return True <* endOfLine
                           _ -> return False <* endOfLine)
              return $ AssemblyInformation (ls1,ls2) pI ps isC) <?>
    "Assembly Information"
  where
    lineAH = "AH" .*> takeWhile (/= '\n') *> endOfLine

parseOrganism = do
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

 
parseCS = do
  fmap (CS . B8.intercalate " ") $
    (many1
     (mkHeader "CO" *> takeWhile1 (/= '\n') <*
      endOfLine) <?> "CON records") <*
    lineTM

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

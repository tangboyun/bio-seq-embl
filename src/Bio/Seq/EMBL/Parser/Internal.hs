{-# LANGUAGE OverloadedStrings, PatternGuards #-}
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

maybeXX :: Parser ()
maybeXX = option "" (fmap B8.concat $ many1 lineXX) *> return ()
  where
    lineXX = "XX" .*> takeWhile (/= '\n') <* endOfLine
    
lineTM :: Parser ()
lineTM = "//" .*> return () <?> "termination line"

parseDC :: Parser DataClass
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

parseTAX :: Parser ByteString
parseTAX = fmap (B8.pack)  $
           count 3 $ satisfy isUpper

parseTOPO :: Parser Topology
parseTOPO = "linear" .*> return Linear <|>
            "circular" .*> return Circular <?>
            "Topology"
            
-- ID   X56734; SV 1; linear; mRNA; STD; PLN; 1859 BP.
-- ID   cel-let-7         standard; RNA; CEL; 99 BP.
parseID :: Parser Identification
parseID = do
  _ <- mkHeader "ID" 
  idf <- takeWhile1 (/= ' ') <* skipWhile (/= ';') <* char ';' <* skipSpace
  sv <- optional $ "SV" .*> skipSpace *>
        decimal <* char ';' 
  topo <- optional $ skipSpace *> parseTOPO <* char ';'
  mol <- skipSpace *> takeWhile1 (/= ';') <*
         char ';'
  dat <- optional $ skipSpace *> parseDC <* char ';'
  tax <- skipSpace *> parseTAX <* char ';'
  len <- skipSpace *> decimal <*
         skipSpace <*. "BP." <* endOfLine
  return $ ID idf sv topo mol dat tax len


parseDT :: Parser Date
parseDT = Date <$> lineDT1 <*> lineDT2
  where 
    parseTimeStr =
      mkHeader "DT" *>
      takeWhile1 (/= ' ') <* skipSpace
    lineDT1 = do
      str1 <- fmap B8.unpack parseTimeStr
      relNum <- fmap Release $
                "(Rel." .*> skipSpace *> decimal <* char ',' <*
                skipSpace <*. "Created)" <* endOfLine
      case toTime str1 of
        Nothing -> fail "Not a valid time string"
        Just t -> return (t,relNum)
    lineDT2 = do
      str2 <- fmap B8.unpack parseTimeStr
      relNum <- fmap Release $
                "(Rel." .*> skipSpace *> decimal <* char ',' <*
                skipSpace <*. "Last updated,"
      verNum <- fmap Version $
                skipSpace *> "Version" .*> skipSpace *> decimal <*
                char ')' <* endOfLine
      case toTime str2 of
        Nothing -> fail "Not a valid time string"
        Just t -> return (t,relNum,verNum)

parseDE :: Parser ByteString
parseDE = do
  fmap (B8.intercalate " ") $
    mkHeader "DE" *> 
    takeWhile1 (/= '\n') `sepBy1`
    (endOfLine *> mkHeader "DE") <* endOfLine

parseKW :: Parser [ByteString]
parseKW = do
  fmap concat $ mkHeader "KW" *>
    (goKW `sepBy1` string "; ") `sepBy1`
    (char ';' *> endOfLine *> mkHeader "KW" ) <* char '.' <* endOfLine
  where
    goKW = takeWhile (\c -> c /= ';' && c /= '.')

parseOS :: Parser (ByteString,Maybe ByteString)
parseOS = do
  _ <- mkHeader "OS"
  des <- fmap (B8.intercalate " ") $
         (fmap trim $ takeWhile1 (\c -> c /= '(' && c /= '\n')) `sepBy1`
         (endOfLine <* mkHeader "OS")
  ogname <- optional $
          -- can handle "OS   Trifolium repens\nOS   (white\nOS   clover)\n"
          fmap (B8.intercalate " ") $
          ((endOfLine *> mkHeader "OS" *> char '(' *>
            (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
             (endOfLine <* mkHeader "OS")) <* char ')') <|>
           (char '(' *> (takeWhile1 (\c -> c /= ')' && c /= '\n') `sepBy1`
                         (endOfLine <* mkHeader "OS")) <* char ')'))
  endOfLine
  return (des,ogname)

parseOC :: Parser [ByteString]
parseOC = do
  fmap concat $
    mkHeader "OC" *>
    (takeWhile1 (\c -> c /= '.' && c /= ';') `sepBy1`
     string "; ") `sepBy1`
    (char ';' *> endOfLine *> mkHeader "OC") <*
    char '.' <* endOfLine

parseOG :: Parser ByteString
parseOG = do
  mkHeader "OG" *>
    takeWhile1 (/= '\n') <* endOfLine

parsePR :: Parser ByteString
parsePR = do
  mkHeader "PR" *> takeWhile (/= ';') <* char ';' <* endOfLine
  

parseAC :: Parser [ByteString]
parseAC = do
  fmap concat $ mkHeader "AC" *>
    (takeWhile1 (/= ';') `sepBy1`
     string "; ") `sepBy1` ( char ';' *> endOfLine *> mkHeader "AC") <*
    char ';' <* endOfLine

parsePaper :: Parser RefLoc
parsePaper = do
  (pub,vol,iss,pg,y) <- mkHeader "RL" *> parseJ
  return $ Paper pub vol iss pg y

parseMisc :: Parser RefLoc
parseMisc = do
  (pub,vol,iss,pg,y) <- mkHeader "RL" *> "(misc) " .*> parseJ
  return $ Misc pub vol iss pg y
  
parseJ :: Parser (ByteString,Int,Maybe Int,ByteString,Int)
parseJ = do
  pub <- fmap (B8.intercalate " ") $
         word `sepBy1` char ' '
  vol <- char ' ' *> option 0 decimal
  iss <- optional $
         char '(' *> decimal <* char ')'
  pg <- char ':' *> takeWhile1 (/= '(')
  y    <- char '(' *> decimal <* char ')' <*
          char '.' <* endOfLine
  return (pub,vol,iss,pg,y)
  where 
    word = takeWhile1
           (\c ->
             isAlpha_ascii c ||
             isDigit c ||
             c == '.' ||
             c == '(' ||
             c == ')') <*
           (do
               c1 <- peekChar
               case c1 of
                 Just ' ' -> return ()
                 _ -> fail "Not word boundary")

parseBook :: Parser RefLoc
parseBook = do
  auths <- fmap concat $
           mkHeader "RL" *> "(in) " .*>
           (as `sepBy1` (char ',' *> endOfLine *> mkHeader "RL")) <*
           char ';' <* endOfLine
  bookName <- fmap (B8.intercalate " ") $
              mkHeader "RL" *>
              (takeWhile (\c -> c /= ':' && c /= '\n') `sepBy1`
               (endOfLine <* mkHeader "RL")) <* char ':'
  pg <- takeWhile1 (/= ';') <* char ';' <* endOfLine
  puber <- fmap (trim . B8.intercalate " ") $
           mkHeader "RL" *>
           takeWhile (\c -> c /= '(' && c /= '\n') `sepBy1`
           (endOfLine <* mkHeader "RL")
  y <- char '(' *> decimal <* char ')' <* char '.' <* endOfLine
  return $ Book bookName auths puber pg y
  where
    as = takeWhile (\c -> c /= ',' && c /= ';') `sepBy1` string ", "

parseSubmitted :: Parser RefLoc
parseSubmitted = do
  timeStr <- fmap B8.unpack $
             mkHeader "RL" *> "Submitted " .*> char '(' *>
             takeWhile1 (/= ')') <* char ')'
  db <- " to the " .*>
        takeWhile1 (/= ' ') <*. " database" <*
        option "" (string "s") <* char '.' <* endOfLine
  addr <- optional $
          fmap (B8.intercalate " ") $
          (mkHeader "RL" *> takeWhile1 (/= '\n')) `sepBy1` endOfLine
  endOfLine
  case toTime timeStr of
    Nothing -> fail "Not a valid time string"
    Just t -> return $ Submitted t db addr

parseThesis :: Parser RefLoc
parseThesis = do
  y <- mkHeader "RL" *> "Thesis " .*>
       char '(' *> decimal <* char ')' <*. ", "
  sch <- fmap (B8.intercalate " ") $
         takeWhile1 (/= '\n') `sepBy1` (endOfLine <* mkHeader "RL")
  endOfLine
  return $ Thesis sch y

parseUnpublished :: Parser RefLoc
parseUnpublished = do
  mkHeader "RL" *> "Unpublished" .*>
    char '.' *> endOfLine *> return Unpublished

parseOther :: Parser RefLoc
parseOther = do
  fmap (Other . B8.init . B8.intercalate " ") $
    mkHeader "RL" *> takeWhile1 (/= '\n') `sepBy1` (endOfLine *> mkHeader "RL") <* endOfLine

parsePatent :: Parser RefLoc
parsePatent = do
  patNum <- mkHeader "RL" *>
            "Patent number " .*> takeWhile1 (/= '-') <* char '-'
  patType <- takeWhile1 (/= '/') <* char '/'
  sn <- decimal <*. ", "
  tStr <- fmap B8.unpack $
          takeWhile1 (/= '.') <* char '.' <* endOfLine
  appCan <- fmap (B8.intercalate " ") $
            (mkHeader "RL" *> takeWhile (/= '\n')) `sepBy1` endOfLine
  endOfLine
  case toTime tStr of
    Nothing -> fail "Not a valid time string"
    Just t -> return $ Patent patNum patType sn t appCan
    
parseRN :: Parser Int
parseRN = do
  mkHeader "RN" *> char '[' *>
    decimal <* char ']' <* endOfLine 

parseRC :: Parser ByteString
parseRC = do
  fmap (B8.intercalate " ") $
    mkHeader "RC" *> takeWhile1 (/= '\n') `sepBy1`
    (endOfLine *> mkHeader "RC") <* endOfLine

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

parseRX :: Parser Resource
parseRX = do
  Resource <$> (mkHeader "RX" *> takeWhile1 isUpper <* string "; ")
           <*> fmap B8.init (takeWhile1 (/= '\n') <* endOfLine)
  
parseRG :: Parser ByteString
parseRG = do
  mkHeader "RG" *> takeWhile1 (/= '\n') <* endOfLine
  
parseRA :: Parser [ByteString]
parseRA = mkHeader "RA" *> 
          oneAuthor `sepBy1`
          (char ',' *> many1 (satisfy (\c -> c == ' ' || c == '\n')) <*
           (optional $ mkHeader "RA")) <*
          char ';' <* endOfLine
  where 
    oneAuthor = fmap (B8.intercalate " ") $
                takeWhile1
                (\c -> c /= ',' && c /= '\n' && c /= ';' && c /= ' ') `sepBy1`
                ((fmap B8.pack $ many1 $ char ' ') <|> (fmap B8.pack $ endOfLine *> mkHeader "RA"))


parseRT :: Parser ByteString
parseRT = empLine <|>
          oneLine <|>
          mulLine <?> "Reference Title"
  where
    empLine = mkHeader "RT" *> optional (char '"') *> char ';' *> endOfLine *> return ""
    oneLine = mkHeader "RT" *> char '"' *>
              title <*
              char '"' <* char ';' <* endOfLine
    mulLine = fmap (trim . B8.intercalate " ") $
              mkHeader "RT" *> char '"' *>
              (title `sepBy1`
               (endOfLine *> mkHeader "RT")) <*
              char '"' <* char ';' <* endOfLine
    title = fmap B8.pack $ many $
            satisfy (\c -> c /= '"' && c /= '\n') <|>
            (char '"' <* (do
                            next <- peekChar
                            case next of
                              Just ';' -> fail "title end"
                              _ -> return ()))
              
    
parseRL :: Parser RefLoc
parseRL = parsePaper <|>
          parseSubmitted <|>
          parsePatent <|>
          parseUnpublished <|>
          parseMisc <|>
          parseBook <|>
          parseThesis <|>
          parseOther <?> "Reference Location"


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

parseCC :: Parser ByteString
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
      k <- mkHeader "FT" *> legalName
      loc <- fmap (B8.intercalate " ") $ skipSpace *>
             (takeWhile1 (/= '\n') `sepBy1` (endOfLine *> conFTLine)) <*
             endOfLine
      return (k,loc)

    parseQV = do
      q <- mkHeader "FT" *> count 16 (satisfy (== ' ')) *>
           char '/' *> legalName
      let func = case q of
            "translation" -> B8.concat
            _ -> B8.intercalate " "

      v <- fmap func $
           char '=' *> (takeWhile1 (/= '\n') `sepBy1`
                        (endOfLine *> conFTLine)) <*
           endOfLine
      return (q,v)
      
    lineFH = "FH" .*> takeWhile (/= '\n') *> endOfLine

parseDR :: Parser DBCrossRef
parseDR = do
  db <- mkHeader "DR" *> takeWhile (/= ';')
  pID <- char ';' *> char ' ' *> takeWhile1 (/= ';') <* char ';' <* char ' '
  sID <- optional $ takeWhile1 (/= '.')
  _ <- char '.' *> endOfLine
  return $ DBCrossRef db pID sID

parseASI :: Parser [AssemblyInformation]
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

parseOrganism :: Parser Organism
parseOrganism = do
  (ogname,cName) <- parseOS
  cs <- parseOC
  maybeXX
  og <- optional parseOG
  return $ Organism ogname cName cs og
  

parseRef :: Parser Reference
parseRef = do
  rn <- fmap RefNo parseRN
  rp <- optional $ parseRP
  rx <- optional $ many1 parseRX
  rg <- optional $ parseRG
  as <- parseRA
  title <- parseRT
  rl <- parseRL
  rc <- optional $ parseRC
  return $ Reference rn rc rp rx rg as title rl

parseCS :: Parser SeqData
parseCS = do
  fmap (CS . B8.intercalate " ") $
    (many1
     (mkHeader "CO" *> takeWhile1 (/= '\n') <*
      endOfLine) <?> "CON records") <*
    lineTM

parseSQ :: Parser SeqData
parseSQ = do
  _ <- mkHeader "SQ" *> "Sequence " .*> (decimal :: Parser Int) <*. " BP;"
  aNum <- char ' ' *> decimal <*. " A;"
  cNum <- char ' ' *> decimal <*. " C;"
  gNum <- char ' ' *> decimal <*. " G;"
  tNum <- char ' ' *> decimal <*. " T;"
  oNum <- char ' ' *> decimal <*. " other;" <* endOfLine
  sdata <- fmap (B8.concat . concat) $
           many1 $
           count 5 (char ' ') *>
           (takeWhile1 isAlpha_ascii `sepBy1` char ' ') <*
           skipSpace <* many1 digit <* endOfLine
  lineTM
  return (SQ (SeqSta aNum cNum gNum tNum oNum) sdata) <?> "Sequence Data"


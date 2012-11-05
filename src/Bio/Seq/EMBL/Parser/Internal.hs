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

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import           Data.Time
import           System.Locale
import           Data.Int
import Bio.Seq.EMBL.Types
import Prelude hiding (takeWhile)
import Data.Word
import Data.Char hiding (isSpace,isDigit)

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

maybeXX = option "" lineXX *> return ()

lineXX = "XX" .*> takeWhile (/= '\n') <* endOfLine
lineTM = "//" .*> return () <?> "termination line"

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
  fmap concat $ goOC <* char '.' <* endOfLine  
  where
    goOC = do
      mkHeader "OC" *>
        (takeWhile1 (\c -> c /= ';' && c /= '.' && c /= '\n') `sepBy1`
         string "; ") `sepBy1` (char ';' *> mkHeader "OC")
        
        
parseOG = do
  mkHeader "OG" *>
    takeWhile1 (/= '\n') <* endOfLine
  
parseProject = do
  mkHeader "PR" *> takeWhile (/= ';') <* char ';' <* endOfLine
  

parseAccession = do
  mkHeader "AC" *>
    ((takeWhile1 (/= ';') <* char ';') `sepBy1`
     many1 (char ' ')) <* endOfLine
  
parsePaper = do
  (pub,vol,iss,pBeg,pEnd,y) <- parseJ
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

    
parseRT = do
  fmap (trim . B8.intercalate " ") $ -- trim for rare case "RT   \"Title\nRT   \";\n"
    ( mkHeader "RT" *> return []  <|> -- empty title
      mkHeader "RT" *> char '"' *>
      (fmap (:[]) $ takeWhile1 $ \c -> c /= '"' && c /= '\n' && c /= ';') <*
      char '"' <|>  -- one line title 
      (do
          line1 <- mkHeader "RT" *> char '"' *> takeWhile1 (/= '\n') <*
                   endOfLine <* mkHeader "RT"
          ls <- takeWhile (\c -> c /= '\n' && c /= '"') `sepBy1`
                (endOfLine *> mkHeader "RT")
          _ <- char '"'
          return $ line1 : ls) -- multi-line title
    ) <* char ';' <* endOfLine
       

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

 
parseCON = do
  fmap (Constructed . B8.intercalate " ") $
    (many1
     (mkHeader "CO" *> takeWhile1 (/= '\n') <*
      endOfLine) <?> "CON records")
                                      
parseDR = fmap DBCrossRef $ 
          mkHeader "DR" *> takeWhile (/= '.') <* char '.' <* endOfLine

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

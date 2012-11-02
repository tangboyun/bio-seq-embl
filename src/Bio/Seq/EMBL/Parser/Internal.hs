{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Low-level Parser
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
import           Data.Time.Format
import           System.Locale
import           Data.Bits
import           Data.Int
import Bio.Seq.EMBL.Types
import Prelude hiding (takeWhile)
import Data.Word
import Data.Char hiding (isSpace,isDigit)

toTime :: String -> Maybe UTCTime
toTime = parseTime defaultTimeLocale "%d-%b-%Y"

trim :: ByteString -> ByteString
trim str = if (== ' ') $ B8.last str
           then trim $ B8.init str
           else str

mkHeader :: ByteString -> Parser String
mkHeader str = str .*> count 3 (satisfy (== ' ')) <?> (B8.unpack str ++ " Line")


lineXX = "XX" .*> takeWhile (/= '\n') *> endOfLine
lineTM = "//" .*> return () <?> "termination line"
  
parseProject = do
  mkHeader "PR" *> "Project:" .*> (decimal <?> "Project Number") <*
    char ';' <* endOfLine


  

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
  iss <- option Nothing
         (fmap Just $
          char '(' *> decimal <* char ')')
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
  addr <- option Nothing $
          fmap (Just . Address . B8.intercalate " ") $
          (mkHeader "RL" *> takeWhile1 (/= '\n')) `sepBy1` endOfLine
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


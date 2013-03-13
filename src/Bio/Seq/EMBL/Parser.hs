{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Parser for EMBL format
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- 
-- 
--
-----------------------------------------------------------------------------

module Bio.Seq.EMBL.Parser
       (
         parseEMBL
       )
       
       where

import           Bio.Seq.EMBL.Parser.Internal
import           Bio.Seq.EMBL.Types
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8

parseEMBL :: Parser EMBL
parseEMBL = do
  idf <- parseID
  maybeXX
  accs <- parseAC
  maybeXX
  proj <- optional $ parsePR <* maybeXX
  dt <- optional $ parseDT <* maybeXX
  des <- optional $ parseDE <* maybeXX
  kws <- optional $ parseKW <* maybeXX
  og <- optional $ parseOrganism <* maybeXX
  refs <- optional $ many1 $ parseRef <* maybeXX
  dr <- optional $ many1 $ parseDR <* maybeXX
  cc <- optional $ parseCC <* maybeXX
  asi <- optional $ parseASI <* maybeXX
  fs <- optional $ parseFT <* maybeXX
  sdata <- parseSQ <|> parseCS
  return $
    EMBL idf accs proj
         dt des kws og refs dr cc asi fs sdata
  


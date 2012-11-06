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
import qualified Data.ByteString.Char8 as B8
import           Data.Char hiding (isSpace,isDigit)
import           Data.Maybe
import           Prelude hiding (takeWhile)

parseEMBL :: Parser SeqRecord
parseEMBL = do
  (acc,sv,topo,mol,dat,tax,len) <- parseID
  maybeXX
  accs <- parseAC
  maybeXX
  project <- optional $ parsePR <* maybeXX
  cr <- lineDT1
  lu <- lineDT2
  maybeXX
  des <- parseDE
  maybeXX
  kws <- parseKW
  maybeXX
  og <- parseOrganism
  maybeXX
  refs <- many1 $ parseRef <* maybeXX
  dr <- optional $ parseDR <* maybeXX
  cc <- optional $ parseCC <* maybeXX
  asi <- optional $ parseASI <* maybeXX
  fs <- parseFT
  maybeXX
  sdata <- parseSQ <|> parseCS
  return $
    SeqRecord acc sv topo mol dat tax len accs project
              cr lu des kws og refs dr cc asi fs sdata
  
    

  


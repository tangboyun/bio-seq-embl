{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module Bio.Seq.EMBL
       (
         extractEMBL
       , extractUnparseable
       , readEMBL
       , splitEMBL
       , module Bio.Seq.EMBL.Parser
       , module Bio.Seq.EMBL.Types
       -- * References
       -- $references

       )
       where

import Bio.Seq.EMBL.Parser
import Bio.Seq.EMBL.Types
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy.Builder as B8
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Maybe

splitEMBL :: ByteString -> [ByteString]
splitEMBL str =  if B8.null str
                 then []
                 else go $ B8.findIndices (== '/') str
  where
    go [] = []
    go (_:[]) = []
    go (i:j:is) =
      if j - i == 1
      then if i > 1 && B8.index str (i-1) == '\n'
           then let (lhs,rhs) = B8.splitAt (j+1) str
                    remain =
                        case B8.dropWhile (/= '\n') rhs of
                            "" -> ""
                            re -> B8.tail re
                in lhs : splitEMBL remain
           else go is
      else go (j:is)

-- | Extract `EMBL` record from a lazy `ByteString` , unparseable parts will be
-- thrown away.
extractEMBL :: ByteString -> [EMBL]
extractEMBL =
  catMaybes .
  map (maybeResult .
       parse parseEMBL) . splitEMBL

readEMBL :: FilePath -> IO [EMBL]
readEMBL fp = B8.readFile fp >>= return . extractEMBL

-- | Lazily extract unparseable record from a large embl file,
--  mainly for debugging purposes.
extractUnparseable :: ByteString -> ByteString
extractUnparseable =
  B8.toLazyByteString .
  foldr (\a b ->
          B8.lazyByteString a `mappend`
          B8.byteString "\n" `mappend`
          b) mempty .
  filter (isNothing . maybeResult .
          parse parseEMBL) . splitEMBL


-- $references
--
-- * The European Nucleotide Archive/EMBL-Bank User Manual:
--   <ftp://ftp.ebi.ac.uk/pub/databases/embl/release/usrman.txt>
--
-- * The DDBJ\/EMBL\/GenBank Feature Table Definition:
--   <ftp://ftp.ebi.ac.uk/pub/databases/embl/doc/FT_current.txt>
--
-- * European Nucleotide Archive: Webin - Features & Qualifiers:
--   <http://www.ebi.ac.uk/ena/WebFeat/>

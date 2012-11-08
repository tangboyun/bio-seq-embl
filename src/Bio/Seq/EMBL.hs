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
import Data.List.Split
import Data.Maybe

splitEMBL :: ByteString -> [ByteString]
splitEMBL str =  if B8.null str
                 then []
                 else go $ B8.findIndices (== '/') str
  where
    go [] = []
    go [i] = []
    go (i:j:is) =
      if j - i == 1
      then let (lhs,rhs) = B8.splitAt (j+1) str
               remain =
                 case B8.dropWhile (/= '\n') rhs of
                   "" -> ""
                   re -> B8.tail re
           in lhs : splitEMBL remain
      else go (j:is)
{-# INLINE splitEMBL #-}

-- | Extract `EMBL` record from a lazy `ByteString` , unparseable parts will be
-- thrown away. You can simply define a function to read embl file.
--
-- @
--    Data.ByteString.Lazy.Char8.readFile fp >>= return . extractSeqRecord
-- @
--
extractEMBL :: ByteString -> [EMBL]
extractEMBL =
  catMaybes .
  map (maybeResult .
       parse parseEMBL) . splitEMBL
  

-- | Lazily extract unparseable record from a large embl file,
--  mainly for debugging purposes.
extractUnparseable :: ByteString -> ByteString
extractUnparseable =
  B8.toLazyByteString .
  foldr (\a b -> B8.lazyByteString a `mappend` b) mempty .
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

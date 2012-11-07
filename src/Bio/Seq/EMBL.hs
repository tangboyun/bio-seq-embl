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

module Bio.Seq.EMBL where

import Bio.Seq.EMBL.Parser
import Bio.Seq.EMBL.Types
import Data.Attoparsec.ByteString.Lazy hiding (takeWhile)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy.Builder as B8
import Data.ByteString.Lazy (ByteString)
import Control.Exception
import Data.Monoid
type Unparseable = Maybe ByteString

readEMBL :: FilePath -> IO ([SeqRecord],Unparseable)
readEMBL fp = do

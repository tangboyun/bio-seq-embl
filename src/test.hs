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

module Main where

import Bio.Seq.EMBL.Parser
import Bio.Seq.EMBL.Types
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import System.Environment
import Control.Monad
import Data.List
import Bio.Seq.EMBL

main = do
  seqs <- readEMBL =<< fmap head getArgs
  -- str <- B8.readFile =<< fmap head getArgs
  -- let (s:ss) = init $ groupBy (\_ b -> B8.take 2 b /= "//")  $ B8.lines $ B8.filter (/= '\r') str
  --     seqs = B8.append (B8.unlines s) "//\n" : map ((flip B8.append "//\n") . B8.unlines . tail) ss
  --     func = flip feed "" . parse parseEMBL . B8.toStrict
  -- forM_ seqs $ \sq -> do
  --   case func sq of
  --     Done _ _ -> return ()
  --     result -> print result >> B8.writeFile "Error.embl" sq >> error "Stop"
  print $ length seqs

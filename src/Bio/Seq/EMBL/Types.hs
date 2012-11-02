{-# LANGUAGE GADTs #-}
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

module Bio.Seq.EMBL.Types

       where

import Data.ByteString (ByteString)
import Data.Time

data Resource = PUBMED ByteString
              | DOI ByteString
              | AGRICOLA ByteString
              deriving (Show,Eq)
                       
data RefLoc where
  Paper ::
    { journal :: Publication
    , volume :: {-# UNPACK #-} !Int
    , issue :: Maybe Int
    , pages :: (Int,Int)
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
  Book ::
    { book :: Publication
    , authors :: [Author]
    , publisher :: Publisher
    , pages :: (Int,Int)
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
  Submitted ::
    { date :: UTCTime
    , database :: Database
    , address :: Maybe Address
    } -> RefLoc
  Unpublished :: RefLoc
  Thesis ::
    { school :: Address
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
  Patent ::
    { patentNumber :: PatentNumber
    , patentType :: PatentType
    , serialNumber :: Int
    , date :: UTCTime
    , applicant :: Applicant
    } -> RefLoc
  Misc ::
    { journal :: Publication
    , volume :: {-# UNPACK #-} !Int
    , issue :: Maybe Int
    , pages :: (Int,Int)
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
   deriving (Show,Eq)

data SeqStatistic = SeqSta 
  { seqlength :: {-# UNPACK #-} !Int
  , numOfA :: {-# UNPACK #-} !Int
  , numOfC :: {-# UNPACK #-} !Int
  , numOfG :: {-# UNPACK #-} !Int
  , numOfT :: {-# UNPACK #-} !Int
  , numOfOthers :: {-# UNPACK #-} !Int
  } deriving (Show,Eq)


newtype Keyword = Keyword ByteString
                deriving (Show,Eq)                  
newtype Author = Author ByteString
               deriving (Show,Eq)
newtype Publisher = Publisher ByteString
                  deriving (Show,Eq)
newtype Publication = Publication ByteString
                    deriving (Show,Eq)
newtype Address = Address ByteString
                deriving (Show,Eq)
newtype Database = Database ByteString
                 deriving (Show,Eq)
newtype PatentNumber = PatentNumber ByteString
                          deriving (Show,Eq)
newtype Applicant = Applicant ByteString
                  deriving (Show,Eq)
newtype PatentType = PatentType ByteString
                   deriving (Show,Eq)

newtype SeqData = SeqData ByteString
                deriving (Show,Eq)

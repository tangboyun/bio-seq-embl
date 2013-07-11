{-# LANGUAGE GADTs, DisambiguateRecordFields #-}
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

data EMBL = EMBL 
  { identification :: Identification
  , accessions :: [ByteString]
  , project :: Maybe ByteString
  , date :: Maybe Date
  , description :: Maybe ByteString
  , keywords :: Maybe [ByteString]
  , organism :: Maybe Organism
  , references :: Maybe [Reference]
  , dbCrossRef :: Maybe [DBCrossRef]
  , comment :: Maybe ByteString
  , assemblyInformation :: Maybe [AssemblyInformation]
  , features :: Maybe [Feature]
  , seqdata :: SeqData
  } deriving (Show,Eq)

data Identification = ID
  { identifier :: ByteString
  , sequenceVersion :: Maybe Int
  , topology :: Maybe Topology
  , moleculeType :: ByteString
  , dataClass :: Maybe DataClass
  , taxonomy :: ByteString
  , seqlength :: Int
  } deriving (Show,Eq)
  
data Date = Date 
  { created :: (UTCTime,Release)
  , lastUpdated :: (UTCTime,Release,Version)
  } deriving (Show,Eq)

data DBCrossRef = DBCrossRef
  { externalDB :: ByteString
  , primaryID :: ByteString
  , secondaryID :: Maybe ByteString
  } deriving (Show,Eq)
             
data Organism = Organism
  { name :: ByteString
  , commonName :: Maybe ByteString
  , classification :: [ByteString]
  , organelle :: Maybe ByteString
  } deriving (Show,Eq)
  
data Reference = Reference 
  { refNo :: RefNo
  , refComment :: Maybe ByteString
  , refPosition :: Maybe [(Int,Int)]
  , refSource :: Maybe [Resource]
  , refGroup :: Maybe ByteString
  , refAuthors :: [ByteString]
  , refTitle :: ByteString
  , refLocation :: RefLoc
  } deriving (Show,Eq)

data Resource = Resource
  { resource :: ByteString
  , resourceIdx :: ByteString
  } deriving (Show,Eq)

data RefLoc where
  Paper ::
    { journal :: ByteString
    , volume :: {-# UNPACK #-} !Int
    , issue :: Maybe Int
    , pages :: ByteString
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
    
  Book ::
    { book :: ByteString
    , authors :: [ByteString]
    , publisher :: ByteString 
    , pages :: ByteString
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
    
  Submitted ::
    { submitDate :: UTCTime
    , database :: ByteString
    , address :: Maybe ByteString
    } -> RefLoc
    
  Unpublished :: RefLoc
  
  Thesis ::
    { school :: ByteString
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
  Patent :: 
    { patentNumber :: ByteString
    , patentType :: ByteString
    , serialNumber :: Int
    , submitDate :: UTCTime
    , applicant :: ByteString
    } -> RefLoc
    
  Misc ::
    { journal :: ByteString
    , volume :: {-# UNPACK #-} !Int
    , issue :: Maybe Int
    , pages :: ByteString
    , year :: {-# UNPACK #-} !Int
    } -> RefLoc
  Other ::
    { otherRefLoc :: ByteString
    } -> RefLoc -- Epub,URL,etc.
   deriving (Show,Eq)

data SeqStatistic = SeqSta 
  { numOfA :: {-# UNPACK #-} !Int
  , numOfC :: {-# UNPACK #-} !Int
  , numOfG :: {-# UNPACK #-} !Int
  , numOfT :: {-# UNPACK #-} !Int
  , numOfO :: {-# UNPACK #-} !Int
  } deriving (Show,Eq)

newtype Version = Version Int
                deriving (Show,Eq)
                         
newtype Release = Release Int
                deriving (Show,Eq)
                         
newtype RefNo = RefNo Int
              deriving (Show,Eq)
                       

data SeqData = SQ SeqStatistic ByteString
             | CS ByteString 
             deriving (Show,Eq)

data Feature = Feature
  { key :: ByteString
  , locationStr :: ByteString
  , valuePairs :: [(ByteString,ByteString)]
  } deriving (Show,Eq)
                        
-- newtype RefComment = RefComment ByteString
--                    deriving (Show,Eq)                  
-- newtype RefGroup = RefGroup ByteString
--                  deriving (Show,Eq)
                         
-- newtype Title = Title ByteString
--               deriving (Show,Eq)                  
-- newtype Keyword = Keyword ByteString
--                 deriving (Show,Eq)                  
-- newtype Author = Author ByteString
--                deriving (Show,Eq)
-- newtype Publisher = Publisher ByteString
--                   deriving (Show,Eq)
-- newtype Publication = Publication ByteString
--                     deriving (Show,Eq)
-- newtype Address = Address ByteString
--                 deriving (Show,Eq)
-- newtype Database = Database ByteString
--                  deriving (Show,Eq)
-- newtype PatentNumber = PatentNumber ByteString
--                           deriving (Show,Eq)
-- newtype Applicant = Applicant ByteString
--                   deriving (Show,Eq)
-- newtype PatentType = PatentType ByteString
--                    deriving (Show,Eq)

-- newtype Key = Key ByteString
--             deriving (Show,Eq)

-- newtype Location = Location ByteString
--                  deriving (Show,Eq)

-- newtype Qualifier = Qualifier ByteString
--                   deriving (Show,Eq)

-- newtype Value = Value ByteString
--                 deriving (Show,Eq)
                         
-- newtype Taxonomy = Taxonomy ByteString
--                  deriving (Show,Eq)
                      

data AssemblyInformation = AssemblyInformation
  { localSpan :: (Int,Int)
  , primaryIdentifier :: ByteString
  , primarySpan :: Maybe (Int,Int)
  , isComplementary :: Bool
  } deriving (Show,Eq)


data Topology = Circular
              | Linear
              deriving (Show,Eq)

data DataClass = CON -- ^ Entry constructed from segment entry sequences;
                     -- if unannotated, annotation may be drawn from
                     -- segment entries
               | PAT -- ^ Patent
               | EST -- ^ Expressed Sequence Tag
               | GSS -- ^ Genome Survey Sequence
               | HTC -- ^ High Thoughput CDNA sequencing
               | HTG -- ^ High Thoughput Genome sequencing
               | MGA -- ^ Mass Genome Annotation
               | WGS -- ^ Whole Genome Shotgun
               | TSA -- ^ Transcriptome Shotgun Assembly
               | STS -- ^ Sequence Tagged Site
               | STD -- ^ Standard (all entries not classified as above)
               | Custom ByteString
               deriving (Show,Eq)
    
                          


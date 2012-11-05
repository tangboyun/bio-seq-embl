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

data Resource = PUBMED ByteString
              | DOI ByteString
              | AGRICOLA ByteString
              deriving (Show,Eq)
                       
data Topology = Circular
              | Linear
              deriving (Show,Eq)
                       
data SeqRecord where
  SeqRecord ::
    { accessionNumber :: ByteString
    , sequenceVersion :: Int
    , topology :: Topology
    , moleculeType :: ByteString
    , dataClass :: DataClass
    , taxonomy :: Taxonomy
    , seqlength :: Int
    , accessions :: [ByteString]
    , project :: Maybe ByteString
    , createdTime :: UTCTime
    , lastUpdatedTime :: UTCTime
    , description :: ByteString
    , keywords :: [Keyword]
    , organism :: Organism
    , references :: [Reference]
    , dbCrossRef :: Maybe ByteString
    , comment :: Maybe ByteString
    , assemblyInformation :: Maybe [AssemblyInformation]
    , features :: [Feature]
    , seqdata :: SeqData
    } -> SeqRecord
    
data Organism = Oranism
  { name :: ByteString
  , commonName :: Maybe ByteString
  , classification :: [ByteString]
  , organelle :: Maybe ByteString
  } deriving (Show,Eq)
  
data Reference where
  Reference ::
    { refNo :: RefNo
    , refComment :: Maybe RefComment
    , refPosition :: Maybe [(Int,Int)]
    , refSource :: Maybe Resource
    , refGroup :: Maybe RefGroup
    , refAuthors :: [Author]
    , refTitle :: Title
    , refLocation :: RefLoc
    } -> Reference
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
  { numOfA :: {-# UNPACK #-} !Int
  , numOfC :: {-# UNPACK #-} !Int
  , numOfG :: {-# UNPACK #-} !Int
  , numOfT :: {-# UNPACK #-} !Int
  , numOfOthers :: {-# UNPACK #-} !Int
  } deriving (Show,Eq)

newtype RefNo = RefNo Int
              deriving (Show,Eq)
newtype RefComment = RefComment ByteString
                   deriving (Show,Eq)                  
newtype RefGroup = RefGroup ByteString
                 deriving (Show,Eq)
                         
newtype Title = Title ByteString
              deriving (Show,Eq)                  
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

data SeqData = SQ SeqStatistic ByteString
             | CN ByteString 
             deriving (Show,Eq)

data Feature = Feature Key Location [(Qualifier,Value)]
               deriving (Show,Eq)
                        
newtype Key = Key ByteString
            deriving (Show,Eq)
newtype Location = Location ByteString
                 deriving (Show,Eq)
newtype Qualifier = Qualifier ByteString
                  deriving (Show,Eq)
newtype Value = Value ByteString
                deriving (Show,Eq)
                         
newtype Constructed = Constructed ByteString
                    deriving (Show,Eq)
                      
newtype DBCrossRef = DBCrossRef ByteString
                     deriving (Show,Eq)

data AssemblyInformation = AssemblyInformation
  { localSpan :: (Int,Int)
  , primaryIdentifier :: ByteString
  , primarySpan :: Maybe (Int,Int)
  , isComplementary :: Bool
  } deriving (Show,Eq)

data DataClass = CON -- ^ Entry constructed from segment entry sequences;
                     -- ^ if unannotated, annotation may be drawn from
                     -- ^ segment entries
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
               deriving (Show,Eq)

data Taxonomy = PHG -- ^ Bacteriophage
              | ENV -- ^ Environmental Sample
              | FUN -- ^ Fungal
              | HUM -- ^ Human
              | INV -- ^ Invertebrate
              | MAM -- ^ Other Mammal
              | VRT -- ^ Other Vertebrate
              | MUS -- ^ Mus musculus
              | PLN -- ^ Plant
              | PRO -- ^ Prokaryote
              | ROD -- ^ Other Rodent
              | SYN -- ^ Synthetic
              | TGN -- ^ Transgenic
              | UNC -- ^ Unclassified
              | VRL -- ^ Viral
              deriving (Show,Eq)

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
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

module Bio.Seq.EMBL.Render
       (
       )
       where

import Bio.Seq.EMBL.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Monoid
import System.Locale
import Data.Time
import Data.Maybe

renderB = render :: ByteString -> Builder

renderAcc = splitToLines "AC" "; " ";\n" "" . accessions

renderDes = fromMaybe mempty .
            fmap (splitToLines "DE" " " "\n" "" . B8.words) .
            description
            
renderKws = fromMaybe mempty .
            fmap (splitToLines "KW" "; " ";\n" "." .
                  map (\(Keyword k) -> k)) .
            keywords



mkHeader :: ByteString -> Builder
mkHeader str = byteString str <>
               byteString "   "

splitOnLineLimit = go [] 0
  where
    lineLimit = 67
    go accLs _ [] = [accLs]
    go accLs accLn ls@(s:ss) =
      let len = accLn + B8.length s
      in if len < lineLimit
         then go (accLs ++ [s]) len ss
         else accLs : go [] 0 ls

splitToLines :: ByteString -> ByteString -> ByteString -> ByteString -> [ByteString] -> Builder
splitToLines lbeg lsep lend lter =
  (<> render lter) . intercalate lend .
  map (\ss ->
        mkHeader lbeg <>
        intercalate lsep (map render ss)
      ) . splitOnLineLimit 
  

intercalate :: ByteString -> [Builder] -> Builder
intercalate str bs = go bs
  where
    bd = byteString str
    go [] = mempty
    go [b] = b
    go (b:cs) = b <> bd <> go cs

class Render a where
  render :: a -> Builder
  renderP :: a -> Builder
  renderP = render
  
instance Render a => Render (Maybe a) where
  render Nothing = mempty
  render (Just a) = render a
  
instance Render Int where
  render = intDec
  
instance Render ByteString where
  render = byteString
  
instance Render String where
  render = stringUtf8
  
instance Render Topology where
  render Circular = byteString "circular"
  render Linear   = byteString "linear"

instance Render Release where
  render (Release i) = render $ "Rel. " ++ show i

instance Render Version where
  render (Version i) = render $ "Version " ++ show i
  
instance Render DataClass where
  render = stringUtf8 . show

instance Render Taxonomy where
  render (Taxonomy str) = render str
  
instance Render Identification where
  render =
    (mkHeader "ID" <>) .
    intercalate "; " . 
    zipWith ($)
    [render . identifier
    ,render . fmap (("SV " ++) . show) .
     sequenceVersion
    ,render . moleculeType
    ,render . dataClass
    ,render . taxonomy
    ,render . (++ " BP.") . show . seqlength
    ] . repeat

instance Render Date where
  render (Date (t1,r1) (t2,r2,ver)) =
    mkHeader "DT" <>
    render (formatTime defaultTimeLocale timeFMT t1) <>
    renderB " (" <> render r1 <> renderB ", Created)\n" <>
    mkHeader "DT" <>
    render (formatTime defaultTimeLocale timeFMT t2) <>
    renderB " (" <> render r2 <> renderB ", Last updated, " <>
    render ver <> renderB ")"
    where
      timeFMT = "%d-%b-%Y"

instance Render Organism where
  render (Organism na com cl og) =
    mkHeader "OS" <>
    render na <>
    fromMaybe mempty 
    (fmap ((renderB " (" <>) .
          (<> renderB ")") .
          render) com) <>
    renderB "\n" <>
    splitToLines "OC" "; " ";\n" "." cl <>
    renderB "\n" <>
    fromMaybe mempty (fmap ((mkHeader "OG" <>) . render) og)

instance Render RefNo where
  render (RefNo i) = mkHeader "RN" <>
                     renderB "[" <>
                     render i <>
                     renderB "]"

instance Render RefComment where
  render (RefComment com) =
    splitToLines "RC" " " "\n" "" $ B8.words com

instance Render Reference where
  render ref =
    zipWith ($)
    [render . refNo
    ,render . refComment
    ,fromMaybe mempty . fmap (mkHeader "" <>   。 map (\(a,b) -> render a <>
                                             renderB "-" <>
                                             render b
                                  ) . refPosition
    
    

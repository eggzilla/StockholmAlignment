-- | Datastructures for Stockholm alignments

module Bio.StockholmData where

import qualified Data.Text as T

-- | Data structure for a Stockholm format alignment
data StockholmAlignment = StockholmAlignment
  { version :: T.Text,
    --annotations with the same tag are merged
    fileAnnotations :: [AnnotationEntry],
    columnAnnotations :: [AnnotationEntry],
    sequenceEntries :: [SequenceEntry]
  }
  deriving (Eq)
           
instance Show StockholmAlignment where
    show (StockholmAlignment _v _f _c _s) = "# STOCKHOLM " ++ T.unpack _v ++ "\n" ++ concatMap (\a -> "#=GF " ++ show a) _f ++ concatMap (showSequenceSpacerEntry spacerLength) _s ++ concatMap (\a -> "#=GC " ++ showAnnotationSpacerEntry spacerLength a) _c ++ "//\n"
     where spacerLength = (1 :: Int) + maximum (map (T.length . sequenceId) _s)

data SequenceEntry = SequenceEntry
  {
    sequenceId :: T.Text,
    entrySequence :: T.Text,
    sequenceAnnotation :: [AnnotationEntry],
    residueAnnotation :: [AnnotationEntry]
  }
  deriving (Show, Eq)

showSequenceSpacerEntry :: Int -> SequenceEntry -> String
showSequenceSpacerEntry spacerLength (SequenceEntry _sid _e _sa _ra) = T.unpack _sid ++ replicate spacerOffsetLength ' ' ++  T.unpack _e ++ "\n"
  where spacerOffsetLength = spacerLength - T.length _sid
           
data AnnotationEntry = AnnotationEntry
  {
    tag :: T.Text,
    annotation :: T.Text
  }
  deriving (Eq)

showAnnotationSpacerEntry :: Int -> AnnotationEntry -> String
showAnnotationSpacerEntry spacerLength (AnnotationEntry _at _aa) = T.unpack  _at ++ replicate spacerOffsetLength ' ' ++ T.unpack _aa ++ "\n"
  where spacerOffsetLength = spacerLength - T.length _at
           
instance Show AnnotationEntry where
    show (AnnotationEntry _at _aa) = T.unpack _at ++ "    " ++ T.unpack _aa ++ "\n"
                                          
data StockholmToken =  TokFileA{ fTag :: T.Text, fInfo :: T.Text } | TokColA { cTag :: T.Text, cInfo :: T.Text  } | TokResA {rId :: T.Text, rTag :: T.Text, rInfo :: T.Text} | TokSeqA {aId :: T.Text, aTag :: T.Text, aInfo :: T.Text} | TokSeq {sId :: T.Text, sSeq :: T.Text} deriving (Show, Eq)


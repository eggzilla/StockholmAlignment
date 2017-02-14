-- | Functions for parsing Stockholm alignments
module Bio.StockholmParser (
                       readExistingStockholm,
                       parseStockholm,
                       readStockholm,
                       module Bio.StockholmData
                      ) where

import Bio.StockholmData
import Text.ParserCombinators.Parsec
import qualified Control.Exception.Base as CE
import qualified Data.Text as T
import Data.List
import System.Directory
import Data.Either.Unwrap

readExistingStockholm :: String -> IO (Either String [StockholmAlignment])
readExistingStockholm filePath =
  if null filePath
    then return (Left "")
    else do
      fileExists <- doesFileExist filePath
      if fileExists
         then do
          stockholmInput <- readStockholm filePath
          if isLeft stockholmInput
             then return (Left (show stockholmInput))
             else return (Right (fromRight stockholmInput))
         else return (Left ("Could not find stockholm alignment file with path:" ++ filePath))

-- | parse 
parseStockholm :: String -> Either ParseError [StockholmAlignment]
parseStockholm input = parse genParseStockholms "Stockholm" input

-- | parse StockholmAlignment from input filePath                      
readStockholm :: String -> IO (Either ParseError [StockholmAlignment])
readStockholm filePath = do
  parsedFile <- parseFromFile genParseStockholms filePath
  CE.evaluate parsedFile

-- | Parse the input as StockholmAlignment
genParseStockholms :: GenParser Char st [StockholmAlignment]
genParseStockholms = do
  alns <- many1 genParseStockholm
  eof
  return alns


-- | Parse the input as StockholmAlignment
genParseStockholm :: GenParser Char st StockholmAlignment
genParseStockholm = do
  string "# STOCKHOLM"
  many1 (try (string " "))
  _version <- many1 (noneOf "\n")
  many (try newline)
  _stockholmToken <- many1 genParseToken
  string "//\n"
  return (tokenToStockholm (T.pack _version) _stockholmToken)

-- | Parse the input as StockholmAlignment datatype
genParseToken :: GenParser Char st StockholmToken
genParseToken = do
  choice [try genParseTokFileA, try genParseTokColA, try genParseTokResA, try genParseTokSeqA, try genParseTokSeq]

genParseTokFileA :: GenParser Char st StockholmToken
genParseTokFileA = do
  many newline
  string "#=GF"
  char ' '
  _tag <- many1 upper
  many1 (char ' ')
  _info <- many1 (noneOf "\n")
  newline
  return (TokFileA (T.pack _tag) (T.pack _info))

genParseTokColA :: GenParser Char st StockholmToken
genParseTokColA = do
  many newline
  string "#=GC"
  char ' '
  _tag <- many1 (noneOf " \n")
  many1 (char ' ')
  _info <- many1 (noneOf "\n")
  newline
  return $ TokColA (T.pack _tag) (T.pack _info)

genParseTokResA :: GenParser Char st StockholmToken
genParseTokResA = do
  many newline
  string "#=GR"
  char ' '
  _id <- many1 (noneOf " \n")
  many1 (char ' ')
  _tag <- many1 (noneOf " \n")
  _info <- many1 (noneOf "\n")
  newline
  return $ TokResA (T.pack _id) (T.pack _tag) (T.pack _info)

genParseTokSeqA :: GenParser Char st StockholmToken
genParseTokSeqA = do
  many newline
  string "#=GS"
  char ' '
  _id <- many1 (noneOf " \n")
  many1 (char ' ')
  _tag <- many1 (noneOf " \n")
  _info <- many1 (noneOf "\n")
  return $ TokSeqA (T.pack _id) (T.pack  _tag) (T.pack _info)

genParseTokSeq :: GenParser Char st StockholmToken
genParseTokSeq = do
  many newline
  _sid <- many1 (noneOf " \n")
  many1 (char ' ')
  _sequence <- many1 (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ-.")
  newline
  return $ TokSeq (T.pack _sid) (T.pack _sequence)

tokenToStockholm :: T.Text -> [StockholmToken] -> StockholmAlignment
tokenToStockholm _version _token = StockholmAlignment _version _fileAnnotation _columnAnnotation _sequenceEntries
  where _fileAtoken = filter isFileTok _token
        _colAtoken = filter isColATok _token
        _resAtoken = filter isResATok _token
        _seqAtoken = filter isSeqATok _token
        _seqtoken = filter isSeqTok _token
        _fileAnnotation = mergeFileToken _fileAtoken
        _columnAnnotation = mergeColToken _colAtoken
        mergedSeqAToken = mergeSeqAToken _seqAtoken
        mergedRAToken = mergeResAToken _resAtoken
        _sequenceEntries = buildSeqEntries mergedSeqAToken mergedRAToken _seqtoken

isFileTok :: StockholmToken -> Bool
isFileTok (TokFileA _ _) = True
isFileTok _ = False

isColATok :: StockholmToken -> Bool
isColATok (TokColA _ _) = True
isColATok _ = False

isResATok :: StockholmToken -> Bool
isResATok (TokResA{}) = True
isResATok _ = False

isSeqATok :: StockholmToken -> Bool
isSeqATok (TokSeqA{}) = True
isSeqATok _ = False

isSeqTok :: StockholmToken -> Bool
isSeqTok (TokSeq _ _) = True
isSeqTok _ = False

mergeFileToken :: [StockholmToken] -> [AnnotationEntry]
mergeFileToken _token = entries
  where tags = nub (map fTag _token)
        entries = map (buildFEntry _token) tags

buildFEntry ::  [StockholmToken] ->  T.Text -> AnnotationEntry
buildFEntry  _token currentTag = entry
  where tagToken = filter (\t -> fTag t == currentTag) _token
        tagInfos = T.concat (map fInfo tagToken)
        entry = AnnotationEntry currentTag tagInfos

mergeColToken :: [StockholmToken] -> [AnnotationEntry]
mergeColToken _token = entries
  where tags = nub (map cTag _token)
        entries = map (buildCEntry _token) tags

buildCEntry :: [StockholmToken] -> T.Text -> AnnotationEntry
buildCEntry _token currentTag = entry
  where tagToken = filter (\t -> cTag t == currentTag) _token
        tagInfos = T.concat (map cInfo tagToken)
        entry = AnnotationEntry currentTag tagInfos

mergeSeqAToken :: [StockholmToken] -> [StockholmToken]
mergeSeqAToken _token = entries
  where aIds = nub (map aId _token)
        entries = concatMap (mergeSAIdToken _token) aIds

mergeSAIdToken :: [StockholmToken] -> T.Text -> [StockholmToken]
mergeSAIdToken _token currentId = tagIdToken
  where idToken = filter (\t -> aId t == currentId) _token
        tags = nub (map aTag idToken)
        tagIdToken = map (mergeSAIdTagToken idToken currentId) tags

mergeSAIdTagToken :: [StockholmToken] ->  T.Text -> T.Text -> StockholmToken
mergeSAIdTagToken _token currentId currentTag = entry
  where tagToken = filter (\t -> aId t == currentId) _token
        tagInfos = T.concat (map aInfo tagToken)
        entry = TokSeqA currentId currentTag tagInfos

mergeResAToken :: [StockholmToken] -> [StockholmToken]
mergeResAToken _token = entries
  where rIds = nub (map rId _token)
        entries = concatMap (mergeRAIdToken _token) rIds

mergeRAIdToken :: [StockholmToken] -> T.Text -> [StockholmToken]
mergeRAIdToken _token currentId = tagIdToken
  where idToken = filter (\t -> rId t == currentId) _token
        tags = nub (map rTag idToken)
        tagIdToken = map (mergeRAIdTagToken idToken currentId) tags

mergeRAIdTagToken :: [StockholmToken] ->  T.Text -> T.Text -> StockholmToken
mergeRAIdTagToken _token currentId currentTag= entry
  where tagToken = filter (\t -> rId t == currentId) _token
        tagInfos = T.concat (map rInfo tagToken)
        entry = TokResA currentId currentTag tagInfos

buildSeqEntries :: [StockholmToken] -> [StockholmToken] -> [StockholmToken] -> [SequenceEntry]
buildSeqEntries  seqA resA _token= entries
  where currentId = map sId _token
        entries = map (buildSeqEntry seqA resA _token) currentId

buildSeqEntry :: [StockholmToken] -> [StockholmToken] -> [StockholmToken] -> T.Text -> SequenceEntry
buildSeqEntry seqAtok resAtok _token currentId = entry
  where idToken = filter (\t -> sId t == currentId ) _token
        idSAToken = filter (\t -> aId t == currentId ) seqAtok
        idRAToken = filter (\t -> rId t == currentId ) resAtok
        seqA = map buildSAEntry idSAToken
        resA = map buildRAEntry idRAToken
        tagInfos = T.concat (map sSeq idToken)
        entry = SequenceEntry currentId tagInfos seqA resA


buildSAEntry :: StockholmToken -> AnnotationEntry
buildSAEntry tok = AnnotationEntry (aTag tok) (aInfo tok)

buildRAEntry :: StockholmToken -> AnnotationEntry
buildRAEntry tok = AnnotationEntry (rTag tok) (rInfo tok)




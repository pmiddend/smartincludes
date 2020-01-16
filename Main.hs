{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<*>), (<|>), many, optional, pure)
import Control.Monad (void)
import Data.Attoparsec.Text
  ( Parser
  , char
  , endOfLine
  , notInClass
  , parseOnly
  , parseOnly
  , sepBy
  , string
  , takeWhile
  )
import Data.Bifunctor (bimap, second)
import Data.Bool (Bool(False, True), (&&), not, otherwise)
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (fold, toList)
import Data.Function ((.), const)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (sort, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Monoid ((<>), mempty)
import Data.Ord (Ord)
import Data.String (String)
import Data.Text (Text, breakOn, intercalate, pack, splitOn)
import qualified Data.Text as Text
import Data.Text.IO (interact)
import Data.Tuple (fst, snd, uncurry)
import Data.Vector
  ( Vector
  , concat
  , drop
  , filter
  , findIndex
  , fromList
  , length
  , mapMaybe
  , null
  , singleton
  , take
  , uniq
  )
import qualified Options.Applicative as Opt
import Prelude ((+))
import System.IO (IO)
import Text.Show (Show)

-- Really general data types and functions
type Endo a = a -> a

type IntRange = (Int, Int)

moveRange :: Int -> Endo IntRange
moveRange x = bimap (+ x) (+ x)

liftPred :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
liftPred op f g x = f x `op` g x

andPred :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andPred = liftPred (&&)

-- Options begin
type Paths = NE.NonEmpty Text

data ProgramOptions =
  ProgramOptions
    { optionsExternalHeader :: Maybe (Paths, Paths)
    , optionsLibraries :: LibraryList
    }

optionsParser :: Opt.Parser ProgramOptions
optionsParser =
  let parseExternalHeader :: String -> (Paths, Paths)
      parseExternalHeader =
        bimap parsePaths parsePaths . second (Text.drop 1) . breakOn "," . pack
      headerArgument =
        optional
          (parseExternalHeader <$>
           Opt.strOption
             (Opt.long "external-header-pair" <>
              Opt.help
                "set a header pair to insert before and after external sources"))
      librariesArgument =
        fromList <$>
        many
          (pack <$>
           Opt.strOption
             (Opt.long "library" <> Opt.help "set a library to sort separately"))
   in ProgramOptions <$> headerArgument <*> librariesArgument

parseOptions :: IO ProgramOptions
parseOptions =
  let opts =
        Opt.info
          (optionsParser Opt.<**> Opt.helper)
          (Opt.fullDesc <> Opt.progDesc "Sort C++ includes smartly")
   in Opt.execParser opts

-- Options end
-- Functions pertaining vectors
pairBlocks :: Vector a -> (a -> Bool) -> (a -> Bool) -> [(Vector a, Vector a)]
pairBlocks xs startPred endPred =
  case findIndex startPred xs of
    Nothing -> [(xs, mempty)]
    Just start ->
      let normalBlock = take start xs
          rest = drop start xs
       in case findIndex endPred rest of
            Nothing -> [(normalBlock, rest)]
            Just end ->
              let specialBlock = take end rest
                  rest' = drop end rest
               in (normalBlock, specialBlock) :
                  pairBlocks rest' startPred endPred

unpairBlocks :: [(Vector a, Vector a)] -> Vector a
unpairBlocks xs = concat (uncurry (<>) <$> xs)

surroundBlocks ::
     forall a. Vector a -> (a -> Bool) -> (a -> Bool) -> a -> a -> Vector a
surroundBlocks input startPred endPred beginSurround endSurround =
  let pairs :: [(Vector a, Vector a)]
      pairs = pairBlocks input startPred endPred
      surround :: Endo (Vector a)
      surround v
        | null v = mempty
        | otherwise = singleton beginSurround <> v <> singleton endSurround
   in unpairBlocks (second surround <$> pairs)

liftVector :: ([a] -> [b]) -> Vector a -> Vector b
liftVector f = fromList . f . toList

sortOnVector :: Ord b => (a -> b) -> Endo (Vector a)
sortOnVector f = liftVector (sortOn f)

sortVector :: Ord a => Endo (Vector a)
sortVector = liftVector sort

type LibraryList = Vector Text

parsePaths :: Text -> Paths
parsePaths = NE.fromList . splitOn "/"

coparsePaths :: Paths -> Text
coparsePaths = intercalate "/" . toList

data Line
  = RemoteIncludeLine Paths
  | NormalLine Text
  deriving (Show, Eq)

remoteIncludeLine :: Line -> Maybe Paths
remoteIncludeLine (RemoteIncludeLine x) = Just x
remoteIncludeLine _ = Nothing

emptyLine :: Line -> Bool
emptyLine (NormalLine x) = Text.null x
emptyLine _ = False

nonEmptyLine :: Line -> Bool
nonEmptyLine = not . emptyLine

isRemoteIncludeLine :: Line -> Bool
isRemoteIncludeLine = isJust . remoteIncludeLine

isNormalLine :: Line -> Bool
isNormalLine = not . isRemoteIncludeLine

parseLines :: Text -> Vector Line
parseLines = fromList . fold . parseOnly (lineParser `sepBy` endOfLine)
  where
    lineParser :: Parser Line
    lineParser = remoteIncludeLineParser <|> normalLineParser
    normalLineParser :: Parser Line
    normalLineParser = NormalLine <$> takeWhile (notInClass "\r\n")
    remoteIncludeLineParser :: Parser Line
    remoteIncludeLineParser = do
      void (string "#include <")
      content <- parsePaths <$> takeWhile (/= '>')
      void (char '>')
      pure (RemoteIncludeLine content)

coparseLines :: Vector Line -> Text
coparseLines = intercalate "\n" . (coparseLine <$>) . toList
  where
    coparseLine :: Line -> Text
    coparseLine (NormalLine l) = l
    coparseLine (RemoteIncludeLine t) = "#include <" <> coparsePaths t <> ">"

processIncludes :: ProgramOptions -> Endo (Vector Line)
processIncludes options lines =
  let includeLinesUnfiltered :: Vector Paths
      includeLinesUnfiltered = mapMaybe remoteIncludeLine lines
      includeLines :: Vector Paths
      includeLines =
        filter
          (maybe
             (const True)
             (\(begin, end) -> (/= begin) `andPred` (/= end))
             (optionsExternalHeader options))
          includeLinesUnfiltered
      lastRank :: Int
      lastRank = length (optionsLibraries options)
      rankInclude :: Paths -> Int
      rankInclude (x :| _) =
        fromMaybe lastRank (findIndex (== x) (optionsLibraries options))
      rankedIncludes :: Vector (Int, Paths)
      rankedIncludes = (\x -> (rankInclude x, x)) <$> includeLines
      sortedIncludes :: Vector (Int, Paths)
      sortedIncludes =
        sortOnVector (\(r, p) -> (r, NE.init p, NE.last p)) rankedIncludes
      withExternalHeader :: Vector (Int, Paths)
      withExternalHeader =
        case optionsExternalHeader options of
          Nothing -> sortedIncludes
          Just (beginExternal, endExternal) ->
            surroundBlocks
              sortedIncludes
              ((== lastRank) . fst)
              ((/= lastRank) . fst)
              (0, beginExternal)
              (0, endExternal)
   in RemoteIncludeLine <$> uniq (snd <$> withExternalHeader)

processFile :: ProgramOptions -> Endo Text
processFile options input =
  let inputLines :: Vector Line
      inputLines = parseLines input
      includeBlocks :: [(Vector Line, Vector Line)]
      includeBlocks = pairBlocks inputLines isRemoteIncludeLine isNormalLine
      processedLines :: Vector Line
      processedLines =
        unpairBlocks (second (processIncludes options) <$> includeBlocks)
   in coparseLines processedLines

main :: IO ()
main = do
  opts <- parseOptions
  interact (processFile opts)

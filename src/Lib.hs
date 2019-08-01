module Lib where

import Data.Char(isUpper)
import Data.Functor((<&>))
import Numeric.Natural (Natural)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Either.Combinators (maybeToRight)
import Control.Monad (foldM)
import Data.Bifunctor (first)

import Html
import Data.Maybe

rawFile :: IO Text
rawFile = TIO.readFile "results.html"  

newtype Score = Score Natural deriving (Show, Eq)

mkScore :: Natural -> Maybe Score
mkScore n
    | 0 <= n && n <= 100 = Just $ Score n
    | otherwise = Nothing


data Domain = Domain {
    _dname :: Text,
    _dscore :: Score,
    _facets :: [Facet]
} deriving Show

data Facet = Facet {
    _fname :: Text,
    _fscore :: Score
} deriving Show

data PersonalityTestResult  = PersonalityTestResult {
    _name :: Text,
    _email :: Text,
    _domains :: [Domain]
} deriving Show

data ParseResult =
    D Text Score | 
    F Text Score |
    Header deriving (Eq, Show)

data ParseError = 
    InvalidNumber Text | 
    ScoreOutOfRange Natural | 
    UnexpectedLine Text deriving (Show, Eq)

data ReconstructionError = FacetWithoutPreceedingDomain Text Score deriving Show

data Err = 
    PE ParseError | 
    RE ReconstructionError | 
    NoMainHtmlBranch | 
    NameNotFound deriving Show

parseLine :: Text -> Either ParseError ParseResult
parseLine "Domain/Facet...... Score" = Right Header
parseLine other = do
    (start, end) <- splitLine other
    scoreNat     <- parseNatural end
    score        <- maybeToRight (ScoreOutOfRange scoreNat) (mkScore scoreNat)
    return $ if Text.all (isUpperOrSpace) start then
            D start score
        else
            F start score
    where
        isUpperOrSpace c = isUpper c || c == ' '



splitLine :: Text -> Either ParseError (Text, Text)
splitLine t = let
    isDot c = c == '.'
    isNonEmpty t = Text.length t > 0
    (firstPart, afterFirstPart) = Text.break isDot t
    (_, lastPart) = Text.span isDot afterFirstPart
    in 
        if isNonEmpty firstPart && isNonEmpty lastPart then
            Right (Text.strip firstPart, Text.strip lastPart)
        else
            Left (UnexpectedLine t)

parseNatural :: Text -> Either ParseError Natural
parseNatural txt = case reads (Text.unpack txt) of
    [(n, "")] -> Right n
    _ -> Left (InvalidNumber txt)

reconstructDomains :: [ParseResult] -> Either ReconstructionError [Domain]
reconstructDomains results =  backwardsResult <&> reverse . (fmap (\d -> d {_facets = reverse (_facets d)}))
    where
        backwardsResult = foldM step [] results
        step :: [Domain] -> ParseResult -> Either ReconstructionError [Domain]
        step xs Header = Right xs
        step xs (D text score) = Right $ (Domain text score []) : xs -- Done with the old domain, start a new one
        step (x : xs) (F text score) = Right $ x { _facets = (Facet text score) : _facets x} : xs -- Add a new facet to existing domain
        step [] (F text score) = Left $ FacetWithoutPreceedingDomain text score 

allDomains :: IO (Either Err [Domain]) 
allDomains = rawFile <&> tree <&> mainBranch <&> fromJust <&> extractEntriesFromRoot <&> map (map (first PE . parseLine)) <&> sequence . concat  <&> (>>= (first RE . reconstructDomains))

extractName :: Text -> Maybe Text
extractName = extractTextBetween "This report compares " "from the country"

extractTextBetween :: Text -> Text -> Text -> Maybe Text
extractTextBetween justBefore justAfter txt = let
    (_, remainder) = Text.breakOn justBefore txt
    startingAtStringCharacter = Text.drop (Text.length justBefore) remainder
    (found, _) =  Text.breakOn justAfter startingAtStringCharacter
    foundStripped = Text.strip found
    in if Text.length foundStripped > 0 then Just foundStripped else Nothing
    
textToPersonalityTestResult :: Text -> Text -> Either Err PersonalityTestResult
textToPersonalityTestResult email t = let
    allPossibleTrees = tree t
    mainBr = maybeToRight NoMainHtmlBranch $ mainBranch allPossibleTrees
    in do
        branch <- mainBr
        let entries = extractEntriesFromRoot branch
        parsed <- traverse (first PE . parseLine) (concat entries)
        reconstructed <- (first RE . reconstructDomains) parsed
        name <- maybeToRight NameNotFound $ extractNameParagraphFromRoot branch >>= extractName
        return $ PersonalityTestResult {
            _name = name,
            _email = email,
            _domains = reconstructed
        }
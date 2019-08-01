module Main where
import System.Environment (getArgs)
import Lib
import Data.Text (Text)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Network.Wreq (Response, responseBody)
import Json () -- TODO orphans
import Send
import Control.Lens ((^.))

main :: IO ()
main = getArgs >>= processFile

processFile :: [FilePath] -> IO ()
processFile [email, f]= do
    rawFile <- TIO.readFile f
    case (textToPersonalityTestResult (Text.pack email) rawFile) of
        Left err -> TIO.putStrLn (errorMessage err)
        Right succ -> do
            TIO.putStrLn $ "Sending " <> tshow (encode (toJSON succ))
            send "https://recruitbot.trikeapps.com/api/v1/roles/bellroy-tech-team-recruit/big_five_profile_submissions" (toJSON succ) >>= printResponse

processFile _ = putStrLn "Usage: personality <email> <personality-html-file>"    
    
errorMessage :: Err -> Text
errorMessage NoMainHtmlBranch = "No main <html> tree found"
errorMessage NameNotFound    = "Name could not be found in HTML"
errorMessage (PE parseError) = "Error extracting data from HTML: " <> parseErrorMessage parseError
errorMessage (RE reconstructError) = "Error reconstructing data: " <> reconstructErrorMessage reconstructError

reconstructErrorMessage :: ReconstructionError -> Text
reconstructErrorMessage (FacetWithoutPreceedingDomain t s) = "Facet without preceeding domain " <> tshow t <> ", " <> tshow s

parseErrorMessage :: ParseError -> Text
parseErrorMessage (InvalidNumber t) = t <> " is an invalid number"
parseErrorMessage (ScoreOutOfRange n) = "Score " <> tshow n <> " is not in the 0-100 range"
parseErrorMessage (UnexpectedLine line) = "Don't know how to interpret '" <> line  <> "'"


printResponse :: Response Text -> IO ()
printResponse resp = TIO.putStrLn (resp ^. responseBody)

tshow :: Show a => a -> Text
tshow = Text.pack . show
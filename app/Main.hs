module Main where
import System.Environment (getArgs)
import Lib
import Data.Text (Text)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as B
import Json

main :: IO ()
main = getArgs >>= processFile

processFile :: [FilePath] -> IO ()
processFile [email, f]= do
    rawFile <- TIO.readFile f
    case (textToPersonalityTestResult (Text.pack email) rawFile) of
        Left err -> TIO.putStrLn (errorMessage err)
        Right succ -> TIO.putStrLn (TE.decodeUtf8 (B.toStrict (encode succ)))

processFile _ = putStrLn "Usage: personality <email> <personality-html-file>"    
    
errorMessage :: Err -> Text
errorMessage _ = "TODO ERROR"
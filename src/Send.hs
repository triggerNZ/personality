module Send where

import Data.Aeson
import Network.Wreq
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as B

send :: String -> Value -> IO (Response Text)
send url payload = do
    resp <- post url payload
    return $ fmap (TE.decodeUtf8 . B.toStrict) resp
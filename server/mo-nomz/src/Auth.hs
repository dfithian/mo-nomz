module Auth where

import Prelude

import Control.Monad (replicateM)
import Crypto.KDF.BCrypt (bcrypt, validatePassword)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray.Encoding (Base(Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.List (find)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Network.Wai (requestHeaders)
import Servant.API ((:>), FromHttpApiData, ToHttpApiData)
import Servant.Server (HasServer, ServerT, err401, hoistServerWithContext, route)
import Servant.Server.Internal.Delayed (addHeaderCheck)
import Servant.Server.Internal.DelayedIO (delayedFailFatal, withRequest)
import System.Random (randomIO)
import qualified Data.ByteString as BS

authorizationHeader :: CI ByteString
authorizationHeader = "X-Mo-Nomz-API-Token"

newtype Authorization = Authorization { unAuthorization :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToField, FromField, ToHttpApiData, FromHttpApiData)

newtype BcryptedAuthorization = BcryptedAuthorization { unBcryptedAuthorization :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToField, FromField, ToHttpApiData, FromHttpApiData)

data Authorized

instance HasServer api context => HasServer (Authorized :> api) context where
  type ServerT (Authorized :> api) m = Authorization -> ServerT api m
  hoistServerWithContext Proxy p nt x = hoistServerWithContext (Proxy :: Proxy api) p nt . x
  route Proxy context subserver = route (Proxy :: Proxy api) context $
    subserver `addHeaderCheck` withRequest headerCheck
    where
      headerCheck = maybe (delayedFailFatal err401) (pure . Authorization . decodeUtf8 . snd) . find ((==) authorizationHeader . fst) . requestHeaders

-- |Returns a plain and a bcrypted API token.
generateToken :: Int -> IO (Authorization, BcryptedAuthorization)
generateToken cost = do
  salt :: ByteString <- BS.pack <$> replicateM 16 randomIO
  userBytes :: ByteString <- BS.pack <$> replicateM 32 randomIO
  let rawToken :: ByteString = convertToBase Base64 userBytes
      bcryptTokenBytes :: ByteString = bcrypt cost salt rawToken
      rawBcryptedToken :: Text = decodeUtf8 $ convertToBase Base64 bcryptTokenBytes
  pure (Authorization $ decodeUtf8 rawToken, BcryptedAuthorization rawBcryptedToken)

-- |Validate a token against its bcrypted version
validateToken :: Authorization -> BcryptedAuthorization -> Either String Bool
validateToken token bcryptedToken =
  let rawToken :: ByteString = encodeUtf8 $ unAuthorization token
      rawBcryptedTokenResult :: Either String ByteString = convertFromBase Base64 . encodeUtf8 . unBcryptedAuthorization $ bcryptedToken
  in validatePassword rawToken <$> rawBcryptedTokenResult

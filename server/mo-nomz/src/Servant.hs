module Servant where

import Data.Aeson (Value)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Servant.API ((:<|>), (:>), Capture, JSON, Post, Raw, ReqBody)

import Types (ParseBlobRequest, ParseBlobResponse, ParseBlobResponseLegacy, ParseLinkRequest, ParseLinkResponse, ParseLinkResponseLegacy)

wholeApi :: Proxy WholeApi
wholeApi = Proxy

nomzApi :: Proxy NomzApi
nomzApi = Proxy

data HTML

type WholeApi =
  NomzApi
    :<|> ".well-known" :> "apple-app-site-association" :> Raw
    :<|> Raw

type NomzApi =
  "api" :> "v1" :> "user" :> Post '[JSON] Value
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" Text :> "ping" :> Post '[JSON] Value

    -- parsing only
    :<|> "api" :> "v1" :> "blob" :> ReqBody '[JSON] ParseBlobRequest :> Post '[JSON] ParseBlobResponse
    :<|> "api" :> "v1" :> "link" :> ReqBody '[JSON] ParseLinkRequest :> Post '[JSON] ParseLinkResponse
    :<|> "api" :> "v2" :> "user" :> Capture "user-id" Text :> "blob" :> ReqBody '[JSON] ParseBlobRequest :> Post '[JSON] ParseBlobResponseLegacy
    :<|> "api" :> "v2" :> "user" :> Capture "user-id" Text :> "link" :> ReqBody '[JSON] ParseLinkRequest :> Post '[JSON] ParseLinkResponseLegacy

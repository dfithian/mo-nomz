module Servant where

import NomzPrelude
import Servant.API ((:<|>), (:>), Capture, Get, JSON, Post, Raw, ReqBody)

import API.Types
  ( GetHealthResponse, ParseBlobRequest, ParseBlobResponse, ParseLinkRequest, ParseLinkResponse
  , UserCreateResponse, UserPingRequest, UserPingResponse
  )
import Auth (Authorized)
import Types (UserId)

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
  "health" :> Get '[JSON] GetHealthResponse
    :<|> "api" :> "v1" :> "user" :> Post '[JSON] UserCreateResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ping" :> ReqBody '[JSON] UserPingRequest :> Post '[JSON] UserPingResponse

    -- parsing only
    :<|> Authorized :> "api" :> "v2" :> "user" :> Capture "user-id" UserId :> "blob" :> ReqBody '[JSON] ParseBlobRequest :> Post '[JSON] ParseBlobResponse
    :<|> Authorized :> "api" :> "v2" :> "user" :> Capture "user-id" UserId :> "link" :> ReqBody '[JSON] ParseLinkRequest :> Post '[JSON] ParseLinkResponse

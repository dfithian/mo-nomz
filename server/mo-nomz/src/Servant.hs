module Servant where

import ClassyPrelude
import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import Servant.API
  ( StdMethod(GET), (:<|>), (:>), Accept, Capture, DeleteNoContent, Get, Header, Headers, JSON
  , MimeRender, OctetStream, Post, PostNoContent, Raw, ReqBody, Verb, contentType, mimeRender
  )
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import API.Types
  ( DeleteGroceryItemRequest, DeleteRecipeRequest, GetHealthResponse, GroceryImportBlobRequest
  , GroceryImportListRequest, ListGroceryItemResponse, ListRecipeResponse, MergeGroceryItemRequest
  , RecipeImportLinkRequest, UpdateGroceryItemRequest, UpdateRecipeRequest, UserCreateResponse
  )
import Auth (Authorized)
import Types (UserId)

wholeApi :: Proxy (NomzApi :<|> Raw)
wholeApi = Proxy

nomzApi :: Proxy NomzApi
nomzApi = Proxy

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML Markup where
  mimeRender _ = renderMarkup

type NomzApi =
  Verb 'GET 307 '[OctetStream] (Headers '[Header "Location" String] ByteString)
    :<|> "index.html" :> Verb 'GET 307 '[OctetStream] (Headers '[Header "Location" String] ByteString)
    :<|> "status" :> Get '[HTML] Markup
    :<|> "health" :> Get '[JSON] GetHealthResponse
    :<|> "api" :> "v1" :> "user" :> Post '[JSON] UserCreateResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> Get '[JSON] ListGroceryItemResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> ReqBody '[JSON] UpdateGroceryItemRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "merge" :> ReqBody '[JSON] MergeGroceryItemRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> ReqBody '[JSON] DeleteGroceryItemRequest :> DeleteNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "clear" :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "list" :> ReqBody '[JSON] GroceryImportListRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "blob" :> ReqBody '[JSON] GroceryImportBlobRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] UpdateRecipeRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Get '[JSON] ListRecipeResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] DeleteRecipeRequest :> DeleteNoContent


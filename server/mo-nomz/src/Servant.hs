module Servant where

import Data.Proxy (Proxy(..))
import Servant.API
  ( (:<|>), (:>), Capture, DeleteNoContent, Get, JSON, Post, PostNoContent, Raw, ReqBody
  )

import API.Types
  ( DeleteGroceryItemRequest, DeleteRecipeRequest, GetHealthResponse, GroceryImportBlobRequest
  , GroceryImportListRequest, ListGroceryItemResponse, ListRecipeResponse, MergeGroceryItemRequest
  , RecipeImportLinkRequest, UpdateRecipeRequest, UserCreateResponse
  )
import Auth (Authorized)
import Types (UserId)

wholeApi :: Proxy (NomzApi :<|> Raw)
wholeApi = Proxy

nomzApi :: Proxy NomzApi
nomzApi = Proxy

type NomzApi =
  "health" :> Get '[JSON] GetHealthResponse
    :<|> "api" :> "v1" :> "user" :> Post '[JSON] UserCreateResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> Get '[JSON] ListGroceryItemResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> ReqBody '[JSON] MergeGroceryItemRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> ReqBody '[JSON] DeleteGroceryItemRequest :> DeleteNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "clear" :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "list" :> ReqBody '[JSON] GroceryImportListRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "blob" :> ReqBody '[JSON] GroceryImportBlobRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] UpdateRecipeRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Get '[JSON] ListRecipeResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] DeleteRecipeRequest :> DeleteNoContent


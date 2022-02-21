module Servant where

import Data.Proxy (Proxy(..))
import Servant.API
  ( (:<|>), (:>), Capture, DeleteNoContent, Get, JSON, Post, PostNoContent, Raw, ReqBody
  )

import API.Types
  ( DeleteGroceryItemRequest, DeleteRecipeRequest, ExportResponse, GetHealthResponse
  , GroceryImportBlobRequest, ListGroceryItemResponse, ListRecipeResponse, ListRecipeResponseV1
  , MergeGroceryItemRequest, ParseBlobRequest, ParseBlobResponse, ParseLinkRequest
  , ParseLinkResponse, ReadableRecipe, RecipeImportLinkRequest, UpdateGroceryItemRequest
  , UpdateRecipeIngredientsRequest, UpdateRecipeRequest, UserCreateResponse, UserPingRequest
  , UserPingResponse
  )
import Auth (Authorized)
import Types (RecipeId, UserId)

wholeApi :: Proxy WholeApi
wholeApi = Proxy

nomzApi :: Proxy NomzApi
nomzApi = Proxy

data HTML

type WholeApi =
  NomzApi :<|> Raw

type NomzApi =
  "health" :> Get '[JSON] GetHealthResponse
    :<|> "api" :> "v1" :> "user" :> Post '[JSON] UserCreateResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ping" :> ReqBody '[JSON] UserPingRequest :> Post '[JSON] UserPingResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> Get '[JSON] ListGroceryItemResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> ReqBody '[JSON] UpdateGroceryItemRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "merge" :> ReqBody '[JSON] MergeGroceryItemRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> ReqBody '[JSON] DeleteGroceryItemRequest :> DeleteNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "clear" :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "grocery" :> "blob" :> ReqBody '[JSON] GroceryImportBlobRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] UpdateRecipeRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "ingredients" :> ReqBody '[JSON] UpdateRecipeIngredientsRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Get '[JSON] ListRecipeResponseV1
    :<|> Authorized :> "api" :> "v2" :> "user" :> Capture "user-id" UserId :> "recipe" :> Get '[JSON] ListRecipeResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Capture "recipe-id" RecipeId :> Get '[JSON] ReadableRecipe
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] DeleteRecipeRequest :> DeleteNoContent

    -- parsing only
    :<|> Authorized :> "api" :> "v2" :> "user" :> Capture "user-id" UserId :> "blob" :> ReqBody '[JSON] ParseBlobRequest :> Post '[JSON] ParseBlobResponse
    :<|> Authorized :> "api" :> "v2" :> "user" :> Capture "user-id" UserId :> "link" :> ReqBody '[JSON] ParseLinkRequest :> Post '[JSON] ParseLinkResponse

    -- export data
    :<|> Authorized :> "api" :> "v2" :> "user" :> Capture "user-id" UserId :> "export" :> Get '[JSON] ExportResponse

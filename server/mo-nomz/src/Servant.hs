module Servant where

import Data.Proxy (Proxy(..))
import Servant.API
  ( (:<|>), (:>), Capture, DeleteNoContent, Get, JSON, Post, PostCreated, PostNoContent, QueryParams
  , ReqBody
  )

import API.Types
  ( ListIngredientResponse, ListRecipeResponse, ListUserResponse, RecipeImportBodyRequest
  , RecipeImportLinkRequest, RecipeImportResponse, UpdateRecipeRequest, UserCreateRequest
  , UserCreateResponse
  )
import Types (RecipeId, UserId)

nomzApi :: Proxy NomzApi
nomzApi = Proxy

type NomzApi =
  "api" :> "v1" :> "user" :> "list" :> Get '[JSON] ListUserResponse
    :<|> "api" :> "v1" :> "user" :> ReqBody '[JSON] UserCreateRequest :> Post '[JSON] UserCreateResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "import" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostCreated '[JSON] RecipeImportResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "import" :> "json" :> ReqBody '[JSON] RecipeImportBodyRequest :> PostCreated '[JSON] RecipeImportResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Capture "recipe-id" RecipeId :> ReqBody '[JSON] UpdateRecipeRequest :> PostNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "list" :> QueryParams "recipe" RecipeId :> Get '[JSON] ListRecipeResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Capture "recipe-id" RecipeId :> DeleteNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> "list" :> QueryParams "recipe" RecipeId :> Get '[JSON] ListIngredientResponse

module Servant where

import Data.Proxy (Proxy(..))
import Servant.API ((:<|>), (:>), Capture, DeleteNoContent, Get, JSON, Post, PostNoContent, ReqBody)

import API.Types
  ( DeleteIngredientRequest, DeleteRecipeRequest, GetHealthResponse, ListIngredientResponse
  , ListRecipeResponse, MergeIngredientRequest, RecipeImportLinkRequest, UpdateRecipeRequest
  , UserCreateRequest, UserCreateResponse
  )
import Types (UserId)

nomzApi :: Proxy NomzApi
nomzApi = Proxy

type NomzApi =
  "health" :> Get '[JSON] GetHealthResponse
    :<|> "api" :> "v1" :> "user" :> ReqBody '[JSON] UserCreateRequest :> Post '[JSON] UserCreateResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> Get '[JSON] ListIngredientResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> ReqBody '[JSON] MergeIngredientRequest :> PostNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> ReqBody '[JSON] DeleteIngredientRequest :> DeleteNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] UpdateRecipeRequest :> PostNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Get '[JSON] ListRecipeResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] DeleteRecipeRequest :> DeleteNoContent

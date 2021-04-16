module Servant where

import Data.Proxy (Proxy(..))
import Servant.API
  ( (:<|>), (:>), Capture, DeleteNoContent, Get, JSON, Post, PostNoContent, Raw, ReqBody
  )

import API.Types
  ( DeleteIngredientRequest, DeleteRecipeRequest, GetHealthResponse, ListIngredientResponse
  , ListRecipeResponse, MergeIngredientRequest, RecipeImportBodyRequest, RecipeImportLinkRequest
  , UpdateRecipeRequest, UserCreateResponse
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
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> Get '[JSON] ListIngredientResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> ReqBody '[JSON] MergeIngredientRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> ReqBody '[JSON] DeleteIngredientRequest :> DeleteNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "body" :> ReqBody '[JSON] RecipeImportBodyRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] UpdateRecipeRequest :> PostNoContent
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> Get '[JSON] ListRecipeResponse
    :<|> Authorized :> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> ReqBody '[JSON] DeleteRecipeRequest :> DeleteNoContent


module Servant where

import Data.Proxy (Proxy(..))
import Servant.API ((:<|>), (:>), Capture, DeleteNoContent, Get, JSON, Post, PostNoContent, ReqBody)

import API.Types
  ( DeleteIngredientRequest, ListIngredientResponse, MergeIngredientRequest, RecipeImportLinkRequest
  , UserCreateRequest, UserCreateResponse
  )
import Types (UserId)

nomzApi :: Proxy NomzApi
nomzApi = Proxy

type NomzApi =
  "api" :> "v1" :> "user" :> ReqBody '[JSON] UserCreateRequest :> Post '[JSON] UserCreateResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "recipe" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> Get '[JSON] ListIngredientResponse
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> ReqBody '[JSON] MergeIngredientRequest :> PostNoContent
    :<|> "api" :> "v1" :> "user" :> Capture "user-id" UserId :> "ingredient" :> ReqBody '[JSON] DeleteIngredientRequest :> DeleteNoContent

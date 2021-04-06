module Servant where

import Data.Proxy (Proxy(..))
import Servant.API
  ( (:<|>), (:>), Capture, DeleteNoContent, Get, JSON, PostCreated, QueryParams, ReqBody
  )

import API.Types
  ( ListIngredientResponse, ListRecipeResponse, RecipeImportBodyRequest, RecipeImportLinkRequest
  , RecipeImportResponse
  )
import Types (RecipeId)

nomzApi :: Proxy NomzApi
nomzApi = Proxy

type NomzApi =
  "api" :> "v1" :> "recipe" :> "import" :> "link" :> ReqBody '[JSON] RecipeImportLinkRequest :> PostCreated '[JSON] RecipeImportResponse
    :<|> "api" :> "v1" :> "recipe" :> "import" :> "json" :> ReqBody '[JSON] RecipeImportBodyRequest :> PostCreated '[JSON] RecipeImportResponse
    :<|> "api" :> "v1" :> "recipe" :> "list" :> QueryParams "recipe" RecipeId :> Get '[JSON] ListRecipeResponse
    :<|> "api" :> "v1" :> "recipe" :> Capture "recipe-id" RecipeId :> DeleteNoContent
    :<|> "api" :> "v1" :> "ingredient" :> "list" :> QueryParams "recipe" RecipeId :> Get '[JSON] ListIngredientResponse

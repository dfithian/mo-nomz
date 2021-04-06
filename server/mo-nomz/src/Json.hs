module Json where

import ClassyPrelude

import Data.Aeson.TH
  ( Options, allNullaryToStringTag, constructorTagModifier, defaultOptions, fieldLabelModifier
  , omitNothingFields
  )

lowerFirst :: String -> String
lowerFirst ((charToLower -> c) : cs) = c : cs
lowerFirst cs = cs

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier     = \field -> lowerFirst . fromMaybe field . stripPrefix prefix $ field
  , constructorTagModifier = lowerFirst
  , allNullaryToStringTag  = True
  , omitNothingFields      = True
  }

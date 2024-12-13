module Types where

data Interactable = Interactable {
  name :: String,
  description :: String
} deriving (Show, Eq)

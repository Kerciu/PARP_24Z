module Interactable where

data Interactable = Interactable {
  name :: String,
  description :: String,
  hint :: String
} deriving (Show, Eq)
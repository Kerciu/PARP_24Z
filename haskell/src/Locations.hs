module Locations where
import Data.Map (Map)
import qualified Data.Map as Map

data Direction = North | South | West | East deriving (Eq, Ord, Show)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West

parseDirection :: String -> Maybe Direction
parseDirection "n" = Just North
parseDirection "north" = Just North
parseDirection "s" = Just South
parseDirection "south" = Just South
parseDirection "w" = Just West
parseDirection "west" = Just West
parseDirection "e" = Just East
parseDirection "east" = Just East


data Location = Location {
    locationName :: String,
    directions :: Map.Map Direction (String, Bool)  -- location name and flag if is unlocked
    }
    deriving Show

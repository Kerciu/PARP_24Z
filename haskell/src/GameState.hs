module GameState where
import Locations

data GameState = GameState {
    currentLocation :: Location,
    gameOver :: Bool
}

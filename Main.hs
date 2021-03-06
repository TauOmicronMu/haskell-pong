module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

fps :: Int
fps = 60

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Haskell Pong" (width, height) (offset, offset)

background, ballColor, paddleColor, wallColor, p1BorderColor, p2BorderColor :: Color
background = black
ballColor = light green
paddleColor = light red
wallColor = greyN 0.75
p1BorderColor = rose
p2BorderColor = yellow

-- Create some aliases for radius and position.
type Radius = Float
type Position = (Float, Float)

radius :: Radius
radius = 10

-- | Holds the state of the current pong game
data GameState = Game {
    ballLoc :: Position, -- ^ (x, y) location of the ball
    ballVel :: Position, -- ^ (x, y) velocity of the ball
    player1 :: Float, -- ^ height of the 1st (left) player's paddle
    player2 :: Float -- ^ height of the 2nd (right) player's paddle
} deriving Show

initialState :: GameState
initialState = Game {
    ballLoc = (-10, 30),
    ballVel = (1, -3),
    player1 = 40,
    player2 = -80
}

-- | Convert a game state into a renderable picture
render :: GameState -> Picture
render game = pictures [ball,
                        walls,
                        genPaddle p1BorderColor 120    $ player1 game,
                        genPaddle p2BorderColor (-120) $ player2 game]
    where 
        -- The pong ball
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid radius
  
        -- The walls at the top and bottom of the screen
        wall :: Float -> Picture
        wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
        
        walls = pictures [wall (150), wall (-150)]

        -- The paddles for each player
        genPaddle :: Color -> Float -> Float -> Picture
        genPaddle borderColor x y = pictures [translate x y $ color borderColor $ rectangleSolid 26 86,
                                              translate x y $ color paddleColor $ rectangleSolid 20 80]

-- | Update the ball position from it's current velocity
moveBall secs game = game { ballLoc = (x', y') }
    where
        -- Previous location and velocity
        (x, y)  = ballLoc game
        (vx, vy) = ballVel game

        --  New locations
        x' = x * vx * secs
        y' = y * vy * secs 

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision 
    where 
        topCollision = y - radius <= -fromIntegral width / 2
        bottomCollision = y + radius >= fromIntegral width / 2

main :: IO()
main = simulate window background fps initialState render update
    where 
        update :: ViewPort -> Float -> GameState -> GameState
        update _ = moveBall 

{- main = animate window background frame
    where
        frame :: Float -> Picture
        frame secs = render $ moveBall secs initialState
-}

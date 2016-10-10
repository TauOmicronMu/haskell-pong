module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Haskell Pong" (width, height) (offset, offset)

background, ballColor, paddleColor :: Color
background = black
ballColor = light green
paddleColor = light red

drawing :: Picture
drawing = pictures [color ballColor   $ circleSolid 30, 
                    color paddleColor $ rectangleSolid 10 50]

main :: IO()
main = display window background drawing




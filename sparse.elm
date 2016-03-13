import Set
import Time
import Graphics.Collage exposing (..)
import Color

-- STATES

glider  = Set.fromList [(1,1),(2,1),(3,1),(3,0),(2,-1)]
acorn   = Set.fromList [(1,0),(3,1),(0,2),(1,2),(4,2),(5,2),(6,2)]
diehard = Set.fromList [(6,0),(0,1),(1,1),(1,2),(5,2),(6,2),(7,2)]
gosper  = Set.fromList [] -- todo

-- LOGIC

main = Signal.map render (Signal.foldp (\_ -> step) acorn (Time.fps 60))

live x state = x `Set.member` state

step state = Set.filter (\x ->
               let cnt  = countLive x state
                   live' = live x state
                   bWhenLive = cnt == 3 || cnt == 4
                   bWhenDead = cnt == 3
               in (live' && bWhenLive) || (not live' && bWhenDead)) (cellsToCheck state)

cellsToCheck = Set.foldl (\x out -> List.foldl Set.insert out (around x)) Set.empty

countLive pos state = List.foldl (\x cnt ->
                        if live x state then cnt + 1 else cnt
                      ) 0 (around pos)

around (x,y) = [
                 (x-1,y-1),(x,y-1),(x+1,y-1),
                 (x-1,y),  (x,y),  (x+1,y),
                 (x-1,y+1),(x,y+1),(x+1,y+1)
               ]

-- GFX

circleSize = 4
cellSize   = 10
width      = 600
height     = 600

render state = Set.toList state |> List.map dot
                                |> collage width height

dot p = circle circleSize |> filled Color.red
                          |> move (pos p)

pos (x,y) = (toFloat (x*cellSize),toFloat (y*cellSize))

import Set

-- STATES

glider  = Set.fromList [(1,1),(2,1),(3,1),(3,0),(2,-1)]
acorn   = Set.fromList [(1,0),(3,1),(0,2),(1,2),(4,2),(5,2),(6,2)]
diehard = Set.fromList [(6,0),(0,1),(1,1),(1,2),(5,2),(6,2),(7,2)]
gosper  = Set.fromList [] -- todo

-- LOGIC

main = lift render (foldp (\_ -> step) diehard (fps 60))

step state = Set.filter (\x ->
               let cnt  = countLive x state
                   live = x `Set.member` state
                   bWhenLive = cnt == 3 || cnt == 4
                   bWhenDead = cnt == 3
               in (live && bWhenLive) || (not live && bWhenDead)) (cellsToCheck state)

cellsToCheck = Set.foldl (\x out -> foldl Set.insert out (around x)) Set.empty

countLive pos state = foldl (\x cnt ->
                        if x `Set.member` state then cnt + 1 else cnt
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

render state = Set.toList state |> map dot
                                |> collage width height

dot p = circle circleSize |> filled red
                          |> move (pos p)

pos (x,y) = (toFloat (x*cellSize),toFloat (y*cellSize))

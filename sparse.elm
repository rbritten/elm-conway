import Set

-- STATES

glider  = [(1,1),(2,1),(3,1),(3,0),(2,-1)]
acorn   = [(1,0),(3,1),(0,2),(1,2),(4,2),(5,2),(6,2)]
diehard = [(6,0),(0,1),(1,1),(1,2),(5,2),(6,2),(7,2)]
gosper  = [] -- todo

-- LOGIC

main = lift render (foldp (\_ -> step) diehard (fps 30))


step state = let set = Set.fromList state
             in filter (\x ->
               let cnt  = countLive x state
                   live = inSet set x
                   bWhenLive = cnt == 3 || cnt == 4
                   bWhenDead = cnt == 3
               in (live && bWhenLive) || (not live && bWhenDead)) (cellsToCheck state)

cellsToCheck = Set.toList . Set.fromList . concatMap around

countLive (x,y) state = let cells = Set.fromList (around (x,y))
                        in length (filter (inSet cells) state)

around (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) (liftL2 (,) [-1..1] [-1..1])

liftL2 f x y = concatMap (\y -> map (\x -> f x y) x) y

inSet set x = Set.member x set

-- GFX

circleSize = 4
cellSize   = 10
width      = 600
height     = 600

render state = collage width height (map dot state)

dot p = move (pos p) (filled red (circle circleSize))

pos (x,y) = (toFloat (x*cellSize),toFloat (y*cellSize))

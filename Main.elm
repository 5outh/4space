import open Maybe
import Char
import Keyboard
import Window
import JavaScript

type Vector4  = (Int, Int, Int, Int)
data Axis = X | Y | Z | W

-- I realize these are both completely isomorphic to True/False, but they're here for clarity.
data Switch = On | Off 
data Parity = Pos | Neg 
data Movement = Movement Axis Parity

type Positioned a = {a | pos     : Vector4 }
type Switchable a = {a | switch  : Switch }

type Prism    = Positioned ( Switchable {} )
type Receptor = Positioned ( Switchable {} )
type Laser    = Positioned ( Switchable { dir : Axis } )
type Floor    = Positioned { rays : Maybe [Axis] }

type Board = { prisms    : [Prism]
             , receptors : [Receptor]
             , lasers    : [Laser]
             , tiles     : [Floor]    -- laser axes
             , minmax    : (Int, Int) -- min/max XYZW values
             }

type Game = { plr   : Positioned {}
            , board : Board
            , won   : Bool 
            }

data Event = SwitchLaser
           | PlayerMove Movement
           | Empty

-- Stream of keyboard events
eventSignal : Signal Event
eventSignal = lift interpretKey Keyboard.lastPressed

interpretKey : Keyboard.KeyCode -> Event
interpretKey k = case Char.fromCode k of
  'W' -> PlayerMove (Movement Y Neg)
  'A' -> PlayerMove (Movement X Neg)
  'S' -> PlayerMove (Movement Y Pos)
  'D' -> PlayerMove (Movement X Pos)
  'I' -> PlayerMove (Movement Z Neg)
  'J' -> PlayerMove (Movement W Neg)
  'K' -> PlayerMove (Movement Z Pos)
  'L' -> PlayerMove (Movement W Pos)
  ' ' -> SwitchLaser 
  _   -> Empty


handleEvent : Event -> Game -> Game
handleEvent e = case e of
  SwitchLaser   -> switchLaser
  PlayerMove  m -> movePlayer m
  Empty         -> id

-- Needs to collect neighbor lasers and toggle them all,
-- which should in turn set floor pieces with lasers on them
-- and trigger any receptors as well.
switchLaser g = g

swap : Switch -> Switch
swap s = if s == On then Off else On

movePlayer : Movement -> Game -> Game
movePlayer m g = 
  let pos'   = boundedMoveVect (g.board.minmax) m (g.plr.pos)
      board' = g.board
      ls     = map (\e -> if e.pos == pos' then {e | switch <- swap e.switch } else e) g.board.lasers
  in if canMoveTo pos' g.board 
     then { g | plr <- { pos = pos'} }
     else { g | board <- {board' | lasers <- ls} }

modifyFloorAt : Vector4 -> (Floor -> Floor) -> Board -> Board
modifyFloorAt v f b = 
  let fs = map (\e -> if e.pos == v then f e else e) b.tiles
  in {b | tiles <- fs}

addLaserAt : Vector4 -> Axis -> Board -> Board
addLaserAt v a b = b

pred : Int -> Int
pred n = n - 1

succ : Int -> Int
succ n = n + 1

neighbors : Vector4 -> [Vector4]
neighbors (x, y, z, w) = 
  let new a = [pred a, a, succ a]
  in new x `lbind` \x' ->
     new y `lbind` \y' ->
     new z `lbind` \z' ->
     new w `lbind` \w' ->
     lreturn (x', y', z', w')

-- Player can move to any space that isn't occupied by a laser or a receptor
canMoveTo : Vector4 -> Board -> Bool
canMoveTo v b = 
     isEmpty ( filter (\e -> e.pos == v) b.receptors )
  && isEmpty ( filter (\e -> e.pos == v) b.lasers    )

-- move a vector, unbounded
moveVect : Movement -> Vector4 -> Vector4
moveVect (Movement axis parity) = 
  mapCoordinate axis <| if parity == Neg then pred else succ

-- move a vector in some direction, bounded by mini and maxi
boundedMoveVect : (Int, Int) -> Movement -> Vector4 -> Vector4
boundedMoveVect (mini, maxi) m v = 
  let (Movement axis parity) = m
      mv = moveVect m v
      c  = getCoordinate axis mv
  in if c < mini || c > maxi then v else mv 

--NB. Doesn't handle EQ.
axisOrder : Axis -> Axis -> Order
axisOrder a b = case (a, b) of
  (X, _) -> LT
  (Y, a) -> if a == X then GT else LT
  (Z, a) -> if a == W then LT else GT
  (W, _) -> GT

setSwitch : Switch -> Switchable a -> Switchable a
setSwitch b s = { s | switch <- b }

mapCoordinate : Axis -> (Int -> Int) -> Vector4 -> Vector4
mapCoordinate a f (x, y, z, w) = case a of
  X -> (f x, y, z, w)
  Y -> (x, f y, z, w)
  Z -> (x, y, f z, w)
  W -> (x, y, z, f w)

getCoordinate : Axis -> Vector4 -> Int
getCoordinate a (x, y, z, w) = case a of
  X -> x
  Y -> y
  Z -> z
  W -> w

setCoordinate : Axis -> Int -> Vector4 -> Vector4
setCoordinate a i = mapCoordinate a (always i)

mapPosCoordinate : Axis -> (Int -> Int) -> Positioned a -> Positioned a
mapPosCoordinate a f p =  
  let pos' = p.pos in {p | pos <- mapCoordinate a f pos' }

setPosCoordinate : Axis -> Int -> Positioned a -> Positioned a
setPosCoordinate a i = mapPosCoordinate a (always i)

getPosCoordinate : Axis -> Positioned a -> Int
getPosCoordinate a {pos} = getCoordinate a pos

zipWith4 : (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f ws xs ys zs = 
  case (ws,xs,ys,zs) of
    (w::ws, x::xs, y::ys, z::zs) -> f w x y z :: zipWith4 f ws xs ys zs
    _ -> []

mergeJusts xs ys = case (xs, ys) of
  ((Just x)::xs',       y ::ys')  -> Just x  :: mergeJusts xs' ys'
  (Nothing ::xs', (Just y)::ys')  -> Just y  :: mergeJusts xs' ys'
  (Nothing ::xs', Nothing ::ys' ) -> Nothing :: mergeJusts xs' ys'
  _                           -> []

inSlice : Vector4 -> (Axis, Axis) -> Positioned a -> Bool
inSlice v (a1, a2) p = 
     getCoordinate a1 v == getPosCoordinate a1 p
  && getCoordinate a2 v == getPosCoordinate a2 p

notAxes : (Axis, Axis) -> [Axis]
notAxes (a, b) = filter (\x -> x /= a && x /= b) [X,Y,Z,W]

iterAxes (a1, a2) is f ps = case is of
  ((a, b)::xs) -> 
    case filter (\p -> getPosCoordinate a1 p == a && getPosCoordinate a2 p == b) ps of
      (z::_) -> Just (f z) :: iterAxes (a1, a2) xs f ps
      []     -> Nothing    :: iterAxes (a1, a2) xs f ps
  []      -> []

groupOn : Int -> [a] -> [[a]]
groupOn n list = case list of 
  [] -> []
  xs -> take n xs :: groupOn n (drop n xs)

showSlice : Positioned {} -> (Axis, Axis) -> Board -> Element
showSlice p (a1, a2) b =
  let [a1', a2']       = sortWith axisOrder <| notAxes (a1, a2)
      (mini, maxi)     = b.minmax
      len              = maxi - mini + 1
      size             = len * 40 + 80 -- width and height
      fbP              = sortBy .pos . filter (inSlice p.pos (a1', a2'))
      (ps, rs, ls, ts) = (fbP b.prisms, fbP b.receptors, fbP b.lasers, fbP b.tiles)
      getAll f xs = 
        let coordPairs = [mini..maxi] `lbind` \x -> [mini..maxi] `lbind` \y -> lreturn (y, x)
        in iterAxes (a1, a2) coordPairs f xs
      boardMap = 
           flow down
        <| map (flow right) 
        <| groupOn len
        <| map (maybe (colorChrElem white '·') id) 
        <| foldr1 mergeJusts 
           [ getAll showLaser    ls
           , getAll showReceptor rs
           , getAll showPrism    ps
           , getAll showFloor    ts
           , getAll showPlayer   [p] ]
      cr pos e = container size size pos e
  in layers [ cr midTop boardMap
            , cr midBottom <| showAxis a1
            , cr midLeft   <| showAxis a2 ]

-- List monad
lreturn x = [x]
lbind = flip concatMap

showGame : Game -> Element
showGame game = flow down 
        <| intersperse (spacer 30 30)
        <| map (flow right)
        <| groupOn 3
        <| planes  `lbind` \p ->
           lreturn <| showSlice game.plr p game.board

{- Begin Debug -}
-- 2 x 2 x 2 x 2 = 16 entities
-- on slice (x, y, 0, 0), this should look like:
-- ?_
-- 
testBoard =
  let ps = [ ]
      rs = [ { switch = Off, pos = (1, 1, 0, 0) } ]
      ls = [ { switch = Off, pos = (0, 1, 0, 0), dir = X } ]
      ts = [ ]
      mm = (0, 3)
  in Board ps rs ls ts mm

testGame = Game {pos = (0,0,0,0)} testBoard False

coordinates = 
  let xs = [0, 1] 
  in  xs `lbind` \x -> 
      xs `lbind` \y ->
      xs `lbind` \z ->
      xs `lbind` \w ->
      lreturn (x, y, z, w)

planes = 
  [ (X, Y)
  , (X, Z)
  , (X, W)
  , (Y, Z)
  , (Y, W)
  , (Z, W) ]

{- End Debug -}

{- Begin Show -}
showPrism : Prism -> Element
showPrism {switch} = colorChrElem green <| case switch of
  On  -> '%'
  Off -> '*'

showReceptor : Receptor -> Element
showReceptor {switch} = colorChrElem blue <| case switch of
  On  -> '$'
  Off -> '_'

-- TODO: Test this out and see if it makes sense.
showLaser : Laser -> Element
showLaser {dir, switch} = colorChrElem red <| case switch of 
  On ->  case dir of
    X -> ':'
    Y -> '='
    Z -> '.'
    W -> ';'
  Off -> '?'

showPlayer : Positioned {} -> Element
showPlayer = always <| colorChrElem darkOrange '@'

showAxis : Axis -> Element
showAxis a =  case a of
  X -> colorSizeChrElem green  25 'X'
  Y -> colorSizeChrElem blue   25 'Y'
  Z -> colorSizeChrElem red    25 'Z'
  W -> colorSizeChrElem purple 25 'W'

-- TODO: showFloor (will depend on player view)
showFloor : Floor -> Element
showFloor = always <| chrElem ' '
{- End Show -}

{- Begin Helpers -}
str : Char -> String
str c = String.cons c ""

chrElem : Char -> Element
chrElem = colorChrElem black

colorChrElem : Color -> Char -> Element
colorChrElem c = colorSizeChrElem c 40

colorSizeChrElem : Color -> Float -> Char -> Element
colorSizeChrElem c h = 
  (width . JavaScript.toInt <| JavaScript.fromFloat h) 
  . text 
  . Text.color c 
  . monospace 
  . Text.height h 
  . toText 
  . str

center : (Int, Int) -> Element -> Element
center (w, h) e = container w h middle e

title : Element
title = flow right <| map (colorSizeChrElem white 80) <| String.toList "4-space"

centeredWithBg : (Int, Int) -> Element -> Element
centeredWithBg (w, h) e = layers 
  [ color darkCharcoal <| container w h middle e
  , container w h midTop (flow up [title, spacer 40 40]) ]

{- End Helpers -}

{- Main -}

main = let testing = False
       in case testing of
        False -> lift2 display Window.dimensions game
        True  -> constant testSignal

display : (Int, Int) -> Game -> Element
display (w, h) g = centeredWithBg (w, h) (showGame g)

game : Signal Game
game = foldp handleEvent testGame eventSignal

testSignal = asText <| neighbors (0,0,0,0)
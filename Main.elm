import open Maybe
import JavaScript

type Vector4  = (Int, Int, Int, Int)
data Axis = X | Y | Z | W
data Switch = On | Off 

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

data Event = SwitchLaser    Switch Vector4 Axis
           | SwitchReceptor Switch Vector4
           | SwitchPrism    Switch Vector4
           | PlayerMove            Vector4

type EventSignal = Signal Event

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

merge : [Char] -> [Char] -> [Char]
merge = zipWith (\x y -> if x == ' ' then y else x)

mergeAll xs = case xs of
  (z::zs) -> merge z (mergeAll zs)
  [z]     -> z
  []      -> []

zipWith4 : (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f ws xs ys zs = 
  case (ws,xs,ys,zs) of
    (w::ws, x::xs, y::ys, z::zs) -> f w x y z :: zipWith4 f ws xs ys zs
    _ -> []

merge4 : [Char] -> [Char] -> [Char] -> [Char] -> [Char]
merge4 = zipWith4 <| 
  \x y z w -> let cs = filter ((/=) ' ') [x, y, z, w]
              in case cs of 
               []    -> ' '
               (c::_) -> c

merge4Justs = zipWith4 <|
  \x y z w -> let cs = filter isJust [x, y, z, w]
              in case cs of 
               []    -> Nothing
               (c::_) -> c

mergeJusts xs ys = case (xs, ys) of
  ((Just x)::xs',       y ::ys')  -> Just x  :: mergeJusts xs' ys'
  (Nothing ::xs', (Just y)::ys')  -> Just y  :: mergeJusts xs' ys'
  (Nothing ::xs', Nothing ::ys' ) -> Nothing :: mergeJusts xs' ys'
  _                           -> []

inSlice : Vector4 -> (Axis, Axis) -> Positioned a -> Bool
inSlice v (a1, a2) p = 
     getCoordinate a1 v == getPosCoordinate a1 p
  && getCoordinate a2 v == getPosCoordinate a2 p

unlines : [String] -> String
unlines xs = case xs of
  []      -> ""
  (s::ss) -> String.append (String.append s "\n") (unlines ss) 

notAxes : (Axis, Axis) -> [Axis]
notAxes (a, b) = filter (\x -> x /= a && x /= b) [X,Y,Z,W]

-- iterate some function over a range of values on a single axis
-- "if something exists at the location in question, return it,
--  otherwise, return Nothing and continue on."
--iterAxis : Axis
--        -> [Int] 
--        -> (Positioned a -> b) 
--        -> [Positioned a] 
--        -> [Maybe b]
iterAxis axis is f ps  = case is of
  (x::xs) -> 
    case filter (\p -> (getPosCoordinate axis p) == x) ps of
      (z::_) -> ( axis, x, Just (f z) ) :: iterAxis axis xs f ps
      []     -> ( axis, x, Nothing )    :: iterAxis axis xs f ps
  []      -> []

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
           flow up
        <| map (flow right) 
        <| groupOn len
        <| map (maybe (colorChrElem grey '·') id) 
        <| foldr1 mergeJusts 
           [ getAll showLaser    ls
           , getAll showReceptor rs
           , getAll showPrism    ps
           , getAll showFloor    ts
           , getAll showPlayer   [p] ]
      cr pos e = container size size pos e
  in layers [ cr middle boardMap
            , cr midBottom <| showAxis a1
            , cr midLeft   <| showAxis a2 ]

-- List monad
lreturn x = [x]
lbind = flip concatMap

main = showGame testGame

showGame : Game -> Element
showGame game = flow down 
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
      mm = (0, 4)
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
  X -> colorSizeChrElem lightGreen  25 'X'
  Y -> colorSizeChrElem lightBlue   25 'Y'
  Z -> colorSizeChrElem lightRed    25 'Z'
  W -> colorSizeChrElem lightPurple 25 'W'

-- TODO: showFloor (will depend on player view)
showFloor : Floor -> Element
showFloor = always <| chrElem ' '

str : Char -> String
str c = String.cons c ""

chrElem : Char -> Element
chrElem = colorChrElem black

colorChrElem : Color -> Char -> Element
colorChrElem c = colorSizeChrElem c 40

colorSizeChrElem : Color -> Float -> Char -> Element
colorSizeChrElem c h = (width . JavaScript.toInt <| JavaScript.fromFloat h) . text . Text.color c . monospace . Text.height h . toText . str
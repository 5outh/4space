import Set
import open Maybe

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
zipWith4 f xs ys zs ws = 
  if isEmpty xs || isEmpty ys || isEmpty zs || isEmpty ws then []
  else f (head xs) (head ys) (head zs) (head ws) :: zipWith4 f (tail xs) (tail ys) (tail zs) (tail ws)

merge4 : [Char] -> [Char] -> [Char] -> [Char] -> [Char]
merge4 = zipWith4 <| 
  \x y z w -> let cs = filter ((/=) ' ') [x, y, z, w]
              in case cs of 
               []    -> ' '
               (c::_) -> c 

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
      (z::_) -> ( (a1, a2), (a, b), Just (f z) ) :: iterAxes (a1, a2) xs f ps
      []     -> ( (a1, a2), (a, b), Nothing    ) :: iterAxes (a1, a2) xs f ps
  []      -> []

-- showSlice : Vector4 -> (Axis, Axis) -> Board -> Element
showSlice v (a1, a2) b =
  let [a1', a2']       = sortWith axisOrder <| notAxes (a1, a2)
      (mini, maxi)     = b.minmax
      fbP              = sortBy .pos . filter (inSlice v (a1', a2'))
      (ps, rs, ls, ts) = (fbP b.prisms, fbP b.receptors, fbP b.lasers, fbP b.tiles)
      getLine  axis x  = filter (\e -> getPosCoordinate axis e == x)
      getLines axis es = map (\i -> getLine axis i es) [mini..maxi]
      formatSection f  = (map . map) f . getLines a2'
      allSections      = zipWith4 merge4
                          (formatSection showPrism    ps)
                          (formatSection showReceptor rs)
                          (formatSection showLaser    ls) 
                          (formatSection showFloor    ts)
      axes = [ String.fromList <| map showAxis [a1, a2] ]
  in [ iterAxes 
        (a1, a2) 
        ([mini..maxi] `lbind` \x -> [mini..maxi] `lbind` \y -> [(x, y)] )
        showLaser
        ls ]

-- (text . monospace . toText) <| unlines <| 

lbind = flip concatMap

main = flow down 
    <| map asText 
    <| coordinates `lbind` \x ->
       planes      `lbind` \p ->
       showSlice x p testBoard 

-- 2 x 2 x 2 x 2 = 16 entities
{- Begin Debug -}
testBoard =
  let ps = [ ]
      rs = [ { switch = Off, pos = (1, 1, 0, 0) } ]
      ls = [ { switch = Off, pos = (0, 1, 0, 0), dir = X } ]
      ts = [ ]
      mm = (0, 1)
  in Board ps rs ls ts mm

coordinates = 
  [ (0, 0, 0, 0)
  , (0, 0, 0, 1)
  , (0, 0, 1, 0)
  , (0, 0, 1, 1)
  , (0, 1, 0, 0)
  , (0, 1, 1, 1)
  , (0, 1, 1, 0)
  , (0, 1, 0, 1)
  , (1, 0, 0, 0)
  , (1, 0, 1, 1)
  , (1, 0, 1, 0)
  , (1, 0, 0, 1)
  , (1, 1, 0, 0)
  , (1, 1, 1, 1)
  , (1, 1, 1, 0)
  , (1, 1, 0, 1) ]

planes = 
  [ (X, Y)
  , (X, Z)
  , (X, W)
  , (Y, Z)
  , (Y, W)
  , (Z, W) ]

{- End Debug -}

showPrism : Prism -> Char
showPrism {switch} = case switch of
  On  -> '%'
  Off -> '*'

showReceptor : Receptor -> Char
showReceptor {switch} = case switch of
  On  -> '$'
  Off -> '_'

-- TODO: Test this out and see if it makes sense.
showLaser : Laser -> Char
showLaser {dir, switch} = case switch of 
  On ->  case dir of
    X -> ':'
    Y -> '='
    Z -> '.'
    W -> ';'
  Off -> '?'

showPlayer : Positioned {} -> Char
showPlayer = always '@'

showAxis : Axis -> Char
showAxis a = case a of
  X -> 'X'
  Y -> 'Y'
  Z -> 'Z'
  W -> 'W'

-- TODO: showFloor (will depend on player view)
showFloor : Floor -> Char
showFloor = always ' '
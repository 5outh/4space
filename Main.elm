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

inSlice : Vector4 -> (Axis, Axis) -> Positioned a -> Bool
inSlice v (a1, a2) p = 
     getCoordinate a1 v == getPosCoordinate a1 p
  && getCoordinate a2 v == getPosCoordinate a2 p

unlines : [String] -> String
unlines xs = case xs of
  []      -> ""
  (s::ss) -> String.append (String.append s "\n") (unlines ss) 

showSlice : Vector4 -> (Axis, Axis) -> Board -> [Element]
showSlice v (a1, a2) b   =
  let [a1', a2']         = sortWith axisOrder [a1, a2]
      (mini, maxi)       = b.minmax
      filterByPosition   = sortBy .pos . filter (inSlice v (a1', a2'))
      prisms'            = filterByPosition b.prisms
      receptors'         = filterByPosition b.receptors
      lasers'            = filterByPosition b.lasers
      tiles'             = filterByPosition b.tiles
      getLine  axis x    = filter (\e -> getPosCoordinate axis e == x)
      getLines axis es   = map (\i -> getLine axis i es) [mini..maxi]
      getCharLists f     = map (map f)
      formatSection f xs = getCharLists f <| getLines a2' xs
      showBoard          = map (text . monospace . toText)
      allSections        = zipWith4 merge4
                            (formatSection showPrism    prisms'    )
                            (formatSection showReceptor receptors' )
                            (formatSection showLaser    lasers'    ) 
                            (formatSection showFloor    tiles'     )
  in showBoard <| map String.fromList <| allSections


p : Prism
p = { switch = Off, pos = (1, 0, 1, 0) }

r : Receptor
r = { switch = Off, pos = (0, 0, 0, 0) }

main = asText <| merge4 [' '] ['$'] [' '] [' ']
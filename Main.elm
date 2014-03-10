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

type Board = { prisms    : Set.Set Prism
             , receptors : Set.Set Receptor
             , lasers    : Set.Set Laser
             , tile      : Maybe (Set.Set Axis) -- laser axes
             }

type Player = Positioned {}

type Game = { plr   : Player
            , board : Board
            , won   : Bool 
            }

data Event = SwitchLaser    Switch Vector4 Axis
           | SwitchReceptor Switch Vector4
           | SwitchPrism    Switch Vector4
           | PlayerMove            Vector4

type EventSignal = Signal Event

setSwitch : Switch -> Switchable a -> Switchable a
setSwitch b s = { s | switch <- b }

mapCoordinate : Axis -> (Int -> Int) -> Vector4 -> Vector4
mapCoordinate a f (x, y, z, w) = case a of
  X -> (f x, y, z, w)
  Y -> (x, f y, z, w)
  Z -> (x, y, f z, w)
  W -> (x, y, z, f w)

setCoordinate : Axis -> Int -> Vector4 -> Vector4
setCoordinate a i = mapCoordinate a (always i)

mapPosCoordinate : Axis -> (Int -> Int) -> Positioned a -> Positioned a
mapPosCoordinate a f p =  
  let pos' = p.pos in {p | pos <- mapCoordinate a f pos' }

setPosCoordinate : Axis -> Int -> Positioned a -> Positioned a
setPosCoordinate a i = mapPosCoordinate a (always i)

merge : [Char] -> [Char] -> [Char]
merge = zipWith (\x y -> if x == ' ' then y else x)

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

showPlayer : Player -> Char
showPlayer = always '@'

-- TODO: showFloor (will depend on player view)

p : Prism
p = { switch = Off, pos = (1, 0, 1, 0) }

r : Receptor
r = { switch = Off, pos = (0, 0, 0, 0) }

main = flow down <| map (asText . mapPosCoordinate W ((+) 1)) [p, setSwitch On p]
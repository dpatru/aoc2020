import Debug.Trace
import Data.Complex


traceIdShow :: Show a => a -> a
traceIdShow x = trace (show x) x

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

type Coord = Complex Float

data Action = Turn Coord
            | Forward Float
            | Move Coord
  deriving Show

data Ship = Ship Coord Coord
  deriving Show

act :: Ship -> Action -> Ship
act (Ship orientation position) (Turn a)
  = traceIdShow $
    trace ("Turning "++ show a) $
    Ship (orientation * a) position
act (Ship orientation position) (Forward d)
  = traceIdShow $
    trace ("Forward "++ show d) $
    Ship orientation (position + orientation * (d:+0))
act (Ship orientation position) (Move p)
  = traceIdShow $
    trace ("Moving "++ show p) $
    Ship orientation (position + p)

-- part 2 changes the orientation to be a waypoint. Because we
-- represent the orientation as a complex number, the changes we make
-- are minimal: we apply moves to the waypoint rather than the ship's
-- position. Also, when calling act, change the start position of the
-- waypoint.
act2 :: Ship -> Action -> Ship
act2 (Ship waypoint position) (Turn a)
  = traceIdShow $
    trace ("Turning "++ show a) $
    Ship (waypoint * a) position
act2 (Ship waypoint position) (Forward d)
  = traceIdShow $
    trace ("Forward "++ show d) $
    Ship waypoint (position + waypoint * (d:+0))
act2 (Ship waypoint position) (Move p)
  = traceIdShow $
    trace ("Moving "++ show p) $
    Ship (waypoint + p) position

main = interact $ (++ "\n") .  show . length
  . tracef ((\d-> "Part 2: " ++ show d)
            . manhatten
            . foldl act2 (Ship (10:+1) (0:+0)))
  . tracef ((\d-> "Part 1: " ++ show d)
            . manhatten
            . foldl act (Ship (1:+0) (0:+0)))
  . map toAction . lines

toAction (a:vs) | a == 'N' = Move $ 0 :+ v
                | a == 'S' = Move $ 0 :+ (-1 * v)
                | a == 'E' = Move $ v :+ 0
                | a == 'W' = Move $ (-1 * v) :+ 0
                | a == 'L' = Turn $  cis $ v / 180 * pi
                | a == 'R' = Turn $  cis $ -1 * v / 180 * pi
                | a == 'F' = Forward $  v
                | otherwise = error $ "unknown action "++ show a
  where v = read vs :: Float

manhatten :: Ship -> Int
manhatten (Ship _ p) = round $ abs(realPart p) + abs(imagPart p)

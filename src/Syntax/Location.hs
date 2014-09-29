module Syntax.Location where

-- Position

data Position = Position
  { positionAbsolute :: Int
  , positionRow      :: Int
  , positionColumn   :: Int
  }
  deriving(Show)

startPosition :: Position
startPosition = Position 0 1 1

updatePosition :: Position -> Char -> Position
updatePosition (Position a r c) '\n' = Position (a+1) (r+1) 1
updatePosition (Position a r c) _ = Position (a+1) r (c+1)

-- Location

data Location = Location
  { locationStart :: Position
  , locationLength :: Int
  }
  deriving(Show)

class Located a where
  locate :: a -> Location

data Locate a = Locate Location a

instance Located (Locate a) where
  locate (Locate l _) = l

instance Show a => Show (Locate a) where
  show (Locate _ a) = show a

delocate :: Locate a -> a
delocate (Locate _ a) = a

makeLocation :: Position -> Position -> Location
makeLocation p p' = Location p (positionAbsolute p' - positionAbsolute p + 1)
module Parsing.Location where

-- Position

data Position = Position
                { positionAbsolute :: Int
                , positionRow      :: Int
                , positionColumn   :: Int
                }

startPosition :: Position
startPosition = Position 0 1 1

-- updatePosition :: Position -> Char -> Position
-- updatePosition (Position a r c) '\n' = Position (a+1) (r+1) 1
-- updatePosition (Position a r c) _ = Position (a+1) r (c+1)

-- showPosition :: String -> Position -> String
-- showPosition fn (Position _ r c) = "File " ++ fn ++ ", line " ++ show r ++ ", characters " ++ show c ++ "-" ++ show c ++ ":\n"

-- Location

data Location = SourceLocation
                { locationStart  :: Position
                , locationLength :: Int
                }
              | NoLocation

-- showLocation :: String -> Location -> String 
-- showLocation fn (Location (Position _ r c) l) = "File " ++ fn ++ ", line " ++ show r ++ ", characters " ++ show c ++ "-" ++ show (c + l - 1) ++ ":\n"

makeLocation :: Position -> Position -> Location
makeLocation p p' = SourceLocation p (positionAbsolute p' - positionAbsolute p + 1)

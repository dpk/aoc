import Data.String

-- this is probably blatant noobish overuse of type aliasing
Coordinate : Type
Coordinate = Integer
Distance : Type
Distance = Integer

data CardinalDirection = North | East | South | West
data RelativeDirection = L | R

record Position where
    constructor MkPosition
    xCoord, yCoord : Coordinate
    facingDirection : CardinalDirection

record Step where
    constructor MkStep
    turnDirection : RelativeDirection
    travelDistance : Distance

startPosition : Position
startPosition = MkPosition 0 0 North

goStraight : Position -> Distance -> Position
goStraight pos dist =
    case (facingDirection pos) of
        North => record { yCoord = ((yCoord pos) + dist) } pos
        East  => record { xCoord = ((xCoord pos) + dist) } pos
        South => record { yCoord = ((yCoord pos) - dist) } pos
        West  => record { xCoord = ((xCoord pos) - dist) } pos

turn : Position -> RelativeDirection -> Position
turn pos change =
    case change of
        L =>
            case (facingDirection pos) of
                North => record { facingDirection = West  } pos
                East  => record { facingDirection = North } pos
                South => record { facingDirection = East  } pos
                West  => record { facingDirection = South } pos
        R =>
            case (facingDirection pos) of
                North => record { facingDirection = East  } pos
                East  => record { facingDirection = South } pos
                South => record { facingDirection = West  } pos
                West  => record { facingDirection = North } pos

distanceToStart : Position -> Distance
distanceToStart pos = (abs $ xCoord pos) + (abs $ yCoord pos)

-- idris sucks
parseStep : String -> Maybe Step
parseStep stepStr =
    case (parseInteger $ strTail stepStr) of
        Just dist =>
            case (strHead stepStr) of
                'L' => Just $ MkStep L dist
                'R' => Just $ MkStep R dist
        Nothing => Nothing

removeSpaces : String -> String
removeSpaces str =
    if str == "" then str else
        if (strHead str) == ' ' then
            removeSpaces $ strTail str
        else
            strCons (strHead str) (removeSpaces $ strTail str)

stepStrings : String -> List String
stepStrings str = split (==',') (removeSpaces str)

steps : String -> List Step
steps stepsStr = catMaybes $ map parseStep $ stepStrings stepsStr

followSteps : Position -> List Step -> Position
followSteps start steps =
    case steps of
        [] => start
        step::restSteps =>
            followSteps (goStraight (turn start $ turnDirection step) $ travelDistance step) restSteps

module WildMisere where

    import Prelude hiding (id)
    import Parser
    import MessageConverter
    import Data.Set hiding (filter, map)

    winner :: String -> Either String (Maybe String)
    winner input = 
        case stringToMoveList input of
            Left error -> Left error
            Right moves
                | duplicatesExist moves -> Left "Found duplicate moves"
                | otherwise ->
                    if winnerExists moves
                    then Right $ Just $ getWinner (reverse moves)
                    else Right Nothing

    getLoser :: [Move] -> String
    getLoser (x:xs) = 
        if winnerExists xs
            then getLoser xs
            else id x

    getWinner :: [Move] -> String
    getWinner moves = getOtherPlayer moves (getLoser moves)

    getOtherPlayer :: [Move] -> String -> String
    getOtherPlayer (m:ms) name = 
        if id m == name
            then getOtherPlayer ms name
            else id m

    stringToMoveList :: String -> Either String [Move]
    stringToMoveList input = 
        case parse input of
            Left error -> Left (show error)
            Right parsedInput -> 
                case jsonToMoves parsedInput [] of 
                    Left error -> Left (show error)
                    Right moves -> Right moves

    winnerExists :: [Move] -> Bool
    winnerExists a = 
        let itGo = betterBoard (board a)
            frozen = any (\x -> x == [Just X, Just X, Just X] || x == [Just O, Just O, Just O]) itGo
        in frozen 

    duplicatesExist :: [Move] -> Bool
    duplicatesExist moves = 
        let coordsList = map c moves
            coordsSet = fromList coordsList
        in 
            length coordsList /= length coordsSet

    betterBoard :: [[Maybe Symbol]] -> [[Maybe Symbol]]
    betterBoard [[a, b, c],[d, e, f],[g, h, i]] = 
        [[a, b, c],[d, e, f],[g, h, i], [a, d, g], [b, e, f], [c, f, i], [a, e, i], [c, e, g]]


    board :: [Move] -> [[Maybe Symbol]]
    board a =   [[getBoardSymbol a (0, 0),
                getBoardSymbol a (0, 1),
                getBoardSymbol a (0, 2)],
                [getBoardSymbol a (1, 0),
                getBoardSymbol a (1, 1),
                getBoardSymbol a (1, 2)],
                [getBoardSymbol a (2, 0),
                getBoardSymbol a (2, 1),
                getBoardSymbol a (2, 2)]]


    getBoardSymbol :: [Move] -> (Int, Int) -> Maybe Symbol
    getBoardSymbol list coords = 
        case filter (\move -> c move == coords) list of
            [] -> Nothing
            (m:_) -> Just (v m)
    
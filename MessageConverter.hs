module MessageConverter where 

    import Parser

    data Move = Move {
        c :: (Int, Int),
        v :: Symbol,
        id :: String
    } deriving Show
    
    data Symbol = X | O deriving (Show, Eq)

    data StructureError = 
        MissingFieldsError
        | ObjectExpectedError
        | WrongCoordinatesStructure 
        | CoordinatesOutOfRangeError 
        | IncorectSymbolError 
        | IdIsNotStringError
    
    instance Show StructureError where
        show MissingFieldsError = "Some required fields are missing"
        show ObjectExpectedError = "An object was expected but wasn't found"
        show WrongCoordinatesStructure = "The structure of the coordinates object is wrong"
        show CoordinatesOutOfRangeError = "The coordinates are out of range"
        show IncorectSymbolError = "An incorrect symbol was found"
        show IdIsNotStringError = "The ID should be a string"

    jsonToMoves :: JsonValue -> [Move] -> Either StructureError [Move]
    jsonToMoves (JsonObject []) acc = Right acc
    jsonToMoves (JsonObject a) acc = 
        let 
            moveData = (getValueByKey "c" a, getValueByKey "v" a, getValueByKey "id" a, getValueByKey "prev" a)
        in
            case moveData of 
                (Just coordinates, Just value, Just idyyyy, prev) -> 
                    case constructMove coordinates value idyyyy of
                        Left error -> Left error
                        Right move -> 
                            case prev of 
                                Nothing -> Right (move:acc)
                                Just (JsonObject p) -> jsonToMoves (JsonObject p) (move:acc)
                                _ -> Left ObjectExpectedError
                _ -> Left MissingFieldsError
    jsonToMoves _ _= Left ObjectExpectedError

    extractCoordinates :: JsonValue -> Either StructureError (Int, Int)
    extractCoordinates (JsonObject coordsObj) = 
        let 
            coordinates = (getValueByKey "0" coordsObj, getValueByKey "1" coordsObj)
        in 
            case coordinates of
                (Just (JsonInt x), Just (JsonInt y)) -> 
                    if x >= 0 && x <= 2 && y >= 0 && y <= 2 
                        then Right (x, y) 
                        else Left CoordinatesOutOfRangeError
                _ -> Left WrongCoordinatesStructure
    extractCoordinates _ = Left ObjectExpectedError

    extractValue :: JsonValue -> Either StructureError Symbol
    extractValue (JsonString a)  
        | a == "x" || a == "X" = Right X
        | a == "o" || a == "O" = Right O
        | otherwise = Left IncorectSymbolError

    extractId :: JsonValue -> Either StructureError String
    extractId (JsonString a) = Right a
    extractId _ = Left IdIsNotStringError

    constructMove :: JsonValue -> JsonValue -> JsonValue -> Either StructureError Move
    constructMove a b c =
        case extractCoordinates a of
            Left coordsError -> Left coordsError
            Right coords ->
                case extractValue b of
                    Left valueError -> Left valueError
                    Right val ->
                        case extractId c of
                            Left idError -> Left idError
                            Right id -> Right $ Move coords val id

    getValueByKey :: String -> [(String, JsonValue)] -> Maybe JsonValue
    getValueByKey _ [] = Nothing
    getValueByKey key ((a, b):xs) = 
        if key == a         
            then Just b 
            else getValueByKey key xs
module Parser where

    import Data.Char 

    data JsonValue = JsonString String | JsonInt Int | JsonObject [(String, JsonValue)]

    --here lies all parsing error messages
    data ParseError = 
          StringParseError 
        | ValueParseError 
        | ObjectParseError
        | MissingCommaError 
        | MissingColonError 
        | MissingClosingBracket 
        | ExtraClosingBracket
        | UnexpectedCharError
    instance Show ParseError where
        show StringParseError = "Failed to parse string"
        show ValueParseError = "Failed to parse JSON value"
        show ObjectParseError = "Failed to parse object"
        show MissingCommaError = "A comma is missing"
        show MissingColonError = "A colon is missing"
        show MissingClosingBracket = "A closing bracket is missing"
        show ExtraClosingBracket = "There is an extra closing bracket"
        show UnexpectedCharError = "An unexpected char was found"
    --
    
    parse :: String -> Either ParseError JsonValue
    parse a = 
        case parseJsonValue a of
            Left error -> Left error
            Right (value, leftValue) -> 
                if allWhiteSpace leftValue 
                    then Right value 
                    else Left UnexpectedCharError       

    parseInt :: String -> (Maybe Int, String)
    parseInt "" = (Nothing, "")
    parseInt (' ':xs) = parseInt xs
    parseInt (a:xs) =
        if isDigit a
             then parseInt' xs (digitToInt a)
             else (Nothing, a:xs)
        where
            parseInt' :: String -> Int -> (Maybe Int, String)
            parseInt' "" acc = (Just acc, "")
            parseInt' (a:xs) acc = 
                if isDigit a 
                    then parseInt' xs (acc*10+digitToInt a)
                    else (Just acc, a:xs)

    parseString :: String -> Either ParseError (String, String)
    parseString "" = Left StringParseError
    parseString (' ':xs) = parseString xs
    parseString ('"':xs) = parseString' xs ""
        where 
            parseString' :: String -> String -> Either ParseError (String, String)
            parseString' ('"':xs) acc = Right (acc, xs)
            parseString' "" acc = Left StringParseError 
            parseString' (a:xs) acc = parseString' xs (acc++[a])
    parseString _ = Left StringParseError

    parseJsonValue :: String -> Either ParseError (JsonValue, String)
    parseJsonValue s = 
        case parseInt s of 
            (Just x, afterParty) -> Right (JsonInt x, afterParty)
            _ -> case parseString s of
                Right (x, afterParty) -> Right (JsonString x, afterParty)
                _ -> case parseObject s of
                    Right (x, afterParty) -> Right (JsonObject x, afterParty)
                    Left err -> Left err

    parseObject :: String -> Either ParseError ([(String, JsonValue)], String)
    parseObject (' ':xs) = parseObject xs
    parseObject ('{':xs) = 
        case eatClosingBracket xs of
            Right afterBracket -> Right ([], afterBracket)
            _ -> parseObject' xs []
            where
                parseObject' :: String -> [(String, JsonValue)] -> Either ParseError ([(String, JsonValue)], String)
                parseObject' ('}':xs) acc = Left ExtraClosingBracket
                parseObject' s acc = 
                    case parseString s of
                        Left err -> Left err
                        Right (key, afterKey) -> 
                            case eatColon afterKey of
                                Left colonError -> Left colonError
                                Right afterColon -> 
                                    case parseJsonValue afterColon of
                                        Left jsonValueError -> Left jsonValueError
                                        Right (value, afterValue) -> 
                                            case eatComma afterValue of
                                                Right afterComma -> parseObject' afterComma ((key, value):acc)
                                                Left jsonCommaError -> 
                                                    case eatClosingBracket afterValue of
                                                        Right afterClosingBracket -> Right ((key, value):acc, afterClosingBracket)
                                                        Left closingBracketError -> Left closingBracketError
    parseObject _ = Left ObjectParseError


    eatComma :: String -> Either ParseError String
    eatComma (',':xs) = Right xs
    eatComma (' ':xs) = eatComma xs
    eatComma _ = Left MissingCommaError

    eatColon :: String -> Either ParseError String
    eatColon (':':xs) = Right xs
    eatColon (' ':xs) = eatColon xs
    eatColon _ = Left MissingColonError

    eatClosingBracket :: String -> Either ParseError String
    eatClosingBracket ('}':xs) = Right xs
    eatClosingBracket (' ':xs) = eatClosingBracket xs
    eatClosingBracket _ = Left MissingClosingBracket

    allWhiteSpace :: String -> Bool
    allWhiteSpace (' ':xs) = allWhiteSpace xs
    allWhiteSpace "" = True
    allWhiteSpace _ = False
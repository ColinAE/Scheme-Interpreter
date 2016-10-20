module ParseScheme where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Control.Monad
    import Control.Monad.Error
    import Numeric
    import Data.Ratio
    import Data.Array
    import Data.Complex
    import LispTypes
    import ParserError
    
    readExpr :: String -> ThrowsError LispVal
    readExpr input = case parse parseExpr "lisp" input of
         Left err -> throwError $ Parserr err
         Right val -> return val
    
    symbol :: Parser Char
    symbol = oneOf "!$%&|*+-/:<=>?@^_~"
    
    spaces :: Parser ()
    spaces = skipMany1 space
    
    escapedChars :: Parser Char
    escapedChars = do char '\\' 
                      x <- oneOf "\\\"nrt" 
                      return $ case x of 
                        '\\' -> x
                        '"'  -> x
                        'n'  -> '\n'
                        'r'  -> '\r'
                        't'  -> '\t'
    
    parseCharacter :: Parser LispVal
    parseCharacter = do
        try $ string "#\\"
        value <- try (string "newline" <|> string "space") <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
        return $ Character $ case value of
            "space" -> ' '
            "newline" -> '\n'
            otherwise -> (value !! 0)
    
    parseString :: Parser LispVal
    parseString = do 
    	char '"'
    	x <- many $ escapedChars <|> noneOf "\"\\"
    	char '"'
    	return $ String x
    
    parseAtom :: Parser LispVal
    parseAtom = do
    	first <- letter <|> symbol
    	rest <- many (letter <|> digit <|> symbol)
    	let atom = first:rest
    	return $ case atom of 
    		"#t" -> Bool True
    		"#f" -> Bool False
    		_    -> Atom atom
    
    parseNumber :: Parser LispVal
    parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin
    
    parseDecimal1 :: Parser LispVal
    parseDecimal1 = many1 digit >>= (return . Number . read)
    
    parseDecimal2 :: Parser LispVal
    parseDecimal2 = do try $ string "#d"
                       x <- many1 digit
                       (return . Number . read) x
    
    parseHex :: Parser LispVal
    parseHex = do try $ string "#x"
                  x <- many1 hexDigit
                  return $ Number (hex2dig x)
    
    parseOct :: Parser LispVal
    parseOct = do try $ string "#o"
                  x <- many1 octDigit
                  return $ Number (oct2dig x)
    
    parseBin :: Parser LispVal
    parseBin = do try $ string "#b"
                  x <- many1 (oneOf "10")
                  return $ Number (bin2dig x)
    
    oct2dig x = fst $ readOct x !! 0
    hex2dig x = fst $ readHex x !! 0
    bin2dig  = bin2dig' 0
    bin2dig' digint "" = digint
    bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                             bin2dig' old xs
    
    parseBool :: Parser LispVal
    parseBool = do
        char '#'
        (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))
    
    parseFloat :: Parser LispVal
    parseFloat = do 
        x <- many1 digit
        char '.'
        y <- many1 digit
        return $ Float (fst.head$readFloat (x++"."++y))
    
    parseRatio :: Parser LispVal
    parseRatio = do 
        x <- many1 digit
        char '/'
        y <- many1 digit
        return $ Ratio ((read x) % (read y))
    
    lispToDouble :: LispVal -> Double
    lispToDouble(Float f) = realToFrac f
    lispToDouble(Number n) = fromIntegral n
    
    parseComplex :: Parser LispVal
    parseComplex = do 
        x <- (try parseFloat <|> parseNumber)
        char '+' 
        y <- (try parseFloat <|> parseNumber)
        char 'i' 
        return $ Complex (lispToDouble x :+ lispToDouble y)
    
    parseList :: Parser LispVal
    parseList = liftM List $ sepBy parseExpr spaces
    
    parseDottedList :: Parser LispVal
    parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail
    
    parseQuoted :: Parser LispVal
    parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]
    
    parseQuasiQuoted :: Parser LispVal
    parseQuasiQuoted = do
        char '`'
        x <- parseExpr
        return $ List [Atom "quasiquote", x]
    
    parseUnQuoted :: Parser LispVal 
    parseUnQuoted = do
        char ','
        x <- parseExpr
        return $ List [Atom "unquote", x]
        
    
    parseVector :: Parser LispVal
    parseVector = do 
        arrayValues <- sepBy parseExpr spaces
        return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)
    
    parseExpr :: Parser LispVal
    parseExpr = parseAtom
            <|> parseString
            <|> try parseRatio
            <|> try parseComplex
            <|> try parseNumber
            <|> try parseBool
            <|> try parseCharacter
            <|> try parseFloat
            <|> try parseQuoted
            <|> try parseQuasiQuoted
            <|> try parseUnQuoted
            <|> try (do 
                    string "#("
                    x <- parseVector
                    char ')'
                    return x)
            <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
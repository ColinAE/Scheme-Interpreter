module EvaluateScheme where
import Control.Monad.Error
import ParserError
import LispTypes

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

{--
peel :: (LispVal -> LispVal) -> [LispVal] -> LispVal
peel f [x] = f x

symbolTest, stringTest, numberTest :: LispVal -> LispVal
symbolTest (Atom "quote") = Bool True
symbolTest _ = Bool False
stringTest (String _) = Bool True
stringTest _ = Bool False
numberTest (Number _) = Bool True
numberTest _ = Bool False

symToStr, strToSym :: LispVal -> LispVal
symToStr (Atom x) = String x
symToStr _ = String ""
strToSym (String x) = Atom x
strToSym _ = Atom ""
--}

primitiveOps :: [(String, [LispVal] -> ThrowsError LispVal)]
primitiveOps = [("+", numericBinop (+)),
                ("-", numericBinop (-)),
                ("*", numericBinop (*)),
                ("/", numericBinop div),
                ("mod", numericBinop mod),
                ("quotient", numericBinop quot),
                ("remainder", numericBinop rem){--,
                ("symbol?", peel symbolTest),
                ("string?", peel stringTest),
                ("number?", peel numberTest),
                ("symbol->string", peel symToStr),
                ("string->symbol", peel strToSym)--}]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func arg = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                       ($ arg)
                       (lookup func primitiveOps)
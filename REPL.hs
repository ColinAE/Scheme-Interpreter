module REPL where
	import Control.Monad
	import System.IO
	import EvaluateScheme
	import ParseScheme
	import ParserError

	printFlush :: String -> IO ()
	printFlush string = putStr string >> hFlush stdout

	prompt :: String -> IO String
	prompt prompt = printFlush prompt >> getLine

	evalPrint :: Env -> String -> IO ()
	evalPrint env expr =  evalString env expr >>= putStrLn
	
	evalString :: Env -> String -> IO String
	evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

	until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
	until_ predicate prompt action = do 
		result <- prompt
		if predicate result 
			then return ()
			else action result >> until_ predicate prompt action

	runOne :: String -> IO()
	runOne expr = nullEnv >>= flip evalPrint expr

	runREPL :: IO ()
	runREPL = nullEnv >>= until_ (== "quit") (prompt "C-Scheme> ") evalPrint


	import Data.IORef

	type Env = IORef [(String, IORef LispVal)]

	nullEnv :: IO Env
	nullEnv = newIORef []

	
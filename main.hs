module Main where
    import System.Environment
    import Control.Monad
    import Control.Monad.Error
    import Text.ParserCombinators.Parsec hiding (spaces)
    import ParserError
    import ParseScheme
    import EvaluateScheme
    import LispTypes
    import REPL
    
    main :: IO ()
    main = do args <- getArgs
              case length args of
                   0 -> runREPL
                   1 -> runOne $ args !! 0
                   otherwise -> putStrLn "Program takes only 0 or 1 argument"
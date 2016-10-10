module Utility where
	import LispTypes

	instance Show LispVal where show = showLispVal

	unwordsList :: [LispVal] -> String
	unwordsList = unwords . map showLispVal

	showLispVal :: LispVal -> String
	showLispVal (String contents) = "\"" ++ contents ++ "\""
	showLispVal (Atom name) = name
	showLispVal (Number contents) = show contents
	showLispVal (Bool True) = "#t"
	showLispVal (Bool False) = "#f"
	showLispVal (List contents) = "(" ++ unwordsList contents ++ ")"
	showLispVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showLispVal tail ++ ")"
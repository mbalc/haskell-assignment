--- Made by mbalc (mb385130) ---

module Main where
import Text.Read

ctrl_prologue = "300 400 translate"
ctrl_failure = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
ctrl_epilogue = "stroke showpage"

--- Syntax ---

data Lexem = Add | Sub | Mul | Div | Moveto | Lineto | Closepath | Translate | Rotate | Liter Integer deriving (Show)

parseWord :: String -> Either String Lexem
parseWord "add" = Right Add
parseWord "sub" = Right Sub
parseWord "mul" = Right Mul
parseWord "div" = Right Div
parseWord "moveto" = Right Moveto
parseWord "lineto" = Right Lineto
parseWord "closepath" = Right Closepath
parseWord "translate" = Right Translate
parseWord "rotate" = Right Rotate
parseWord s = case (readEither s :: Either String Integer) of
  Left o -> Left o
  Right a -> Right (Liter a)


parseInput :: String -> Either String [Lexem]
parseInput input = mapM parseWord (words input)


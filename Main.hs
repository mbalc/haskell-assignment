--- Made by mbalc (mb385130) ---

module Main where
import Lib
import Text.Read

ctrl_prologue = "300 400 translate"
ctrl_failure = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
ctrl_epilogue = "stroke showpage"

--- Syntax ---

data Lexem = Add | Sub | Mul | Div | Moveto | Lineto | Closepath | Translate | Rotate | Liter Integer deriving (Show)

--- Parsing ---

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

--- Semantics ---

type Stack = [R]
type State = (Picture, Stack)

alterStack :: (R -> R -> R) -> State -> Either String State
alterStack op (p, a : b : tl) = Right (p, (op a b) : tl)
alterStack op _ = Left "stack underflow"

progress :: State -> Lexem -> Either String State
progress (p, s) l = case l of
  Liter n -> Right (p, (toRational n) : s)
  Add -> alterStack (+) (p, s)
  Sub -> alterStack (-) (p, s)
  Mul -> alterStack (*) (p, s)
  Div -> alterStack (/) (p, s)

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
data State = State { pic :: Picture
                   , stack :: Stack
                   }

pop :: Stack -> Either String (R, Stack)
pop (a : tl) = Right (a, tl)
pop _ = Left "stack underflow"

requireTwo :: Stack -> Either String (R, R, Stack)
requireTwo st = do
  (a, st') <- pop st
  (b, st'') <- pop st'
  return (a, b, st'')

alterStack :: (R -> R -> R) -> State -> Either String State
alterStack op state = do
  (a, b, tl) <- requireTwo (stack state)
  return state {stack = (op a b) : tl}


progress :: State -> Lexem -> Either String State
progress state l = case l of
  Liter n -> let sts = stack state in Right (state {stack = (toRational n) : sts})
  Add -> alterStack (+) state
  Sub -> alterStack (-) state
  Mul -> alterStack (*) state
  Div -> alterStack (/) state

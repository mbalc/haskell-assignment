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
                   , currPoint :: Maybe Point
                   , lastPathPoint :: Maybe Point
                   }

requireCurrPt :: State -> Either String Point
requireCurrPt state = case currPoint state of
  Nothing -> Left "Missing current point"
  Just pt -> Right pt

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

moveto :: State -> Either String State
moveto state = do
  (a, b, tl) <- requireTwo (stack state)
  let newPt = Point (a, b)
  return state { stack = tl
               , currPoint = Just newPt
               , lastPathPoint = Just newPt
               }

lineto :: State -> Either String State
lineto state = do
  (a, b, tl) <- requireTwo (stack state)
  Point cur <- requireCurrPt state
  let Point newPtCoords = Point (a, b)
  return state { stack = tl
               , pic = oldPic & (line cur newPtCoords)
               , currPoint = Just (Point newPtCoords)
               } where oldPic = pic state


progress :: State -> Lexem -> Either String State
progress state l = case l of
  Liter n -> let sts = stack state in Right (state {stack = (toRational n) : sts})
  Add -> alterStack (+) state
  Sub -> alterStack (-) state
  Mul -> alterStack (*) state
  Div -> alterStack (/) state
  Moveto -> moveto state
  Lineto -> lineto state

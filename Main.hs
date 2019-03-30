--- Made by mbalc (mb385130) ---

module Main where
import Lib
import Text.Read
import Control.Monad (foldM)
import System.Environment (getArgs)

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
                   } deriving (Show)

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
  return (b, a, st'')

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

closepath :: State -> Either String State -- dirty, sorry
closepath state = do
  case lastPathPoint state of
    Nothing -> Right state
    Just (Point last) -> case currPoint state of
      Nothing -> Right state
      Just (Point cur) -> case cur == last of
        True -> Right state
        False -> Right state { pic = oldPic & (line cur last)
                             , currPoint = Just (Point last)
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
  Closepath -> closepath state

--- Input & Output ---

ctrl_prologue = "300 400 translate\n"
ctrl_failure = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\n"
ctrl_epilogue = "stroke showpage"

renderPic :: IntRendering -> String
renderPic = foldl (
  \acc ((x1, y1), (x2, y2)) ->
    acc ++ (show x1) ++ " " ++ (show y1) ++ " moveto " ++ (show x2) ++ " " ++ (show(y2)) ++ " lineto\n"
  ) ""


main :: IO ()
main = do
  args <- getArgs
  let maybeScale = if length args == 1 then readMaybe $ head args :: Maybe Int else Just 1
  input <- getContents
  let eitherLastState = parseInput input >>= foldM progress initialState

  putStrLn ctrl_prologue
  case (eitherLastState, maybeScale) of
    (Left _, _) -> putStrLn ctrl_failure
    (_, Nothing) -> putStrLn ctrl_failure
    (Right lastState, Just scale) -> putStrLn (renderPic $ renderScaled scale (pic lastState))
  putStrLn ctrl_epilogue


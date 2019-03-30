--- Made by mbalc (mb385130) ---

module Lib where
import Mon
import Data.Fixed(mod')

--- ---  RenderData  --- ---
type R = Rational

type R2 = (R, R)

data Vec = Vec R2                     -- wektor 2D
data Point = Point R2 deriving (Show) -- punkt 2D

instance Eq Vec where
  Vec a == Vec b = a == b

instance Eq Point where
  Point a == Point b = a == b

instance Show Vec where
  show (Vec v) = "vec" ++ show v

point :: R2 -> Point
point coords = Point coords

vec :: R2 -> Vec
vec coords = Vec coords

instance Mon Vec where
  m1 = Vec (0, 0)
  (Vec (a, b)) >< (Vec (p, q)) = Vec ((a + p), (b + q))


type PicLine = (Point, Point)
data Picture = Picture [PicLine] deriving (Show)

picLine :: (R, R) -> (R, R) -> PicLine
picLine (x1, y1) (x2, y2) = (Point (x1, y1), Point (x2, y2))


-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R, R) -> (R, R) -> Picture
line (x1, y1) (x2, y2) = Picture [(Point (x1, y1), Point (x2, y2))]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0, 0)
rectangle :: R -> R -> Picture
rectangle a b = Picture [ picLine (0, 0) (a, 0)
                        , picLine (0, 0) (0, b)
                        , picLine (a, 0) (a, b)
                        , picLine (0, b) (a, b)
                        ]

-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
(Picture a) & (Picture b) = Picture (a ++ b)

type IntPoint = (Int, Int)
type IntLine = (IntPoint, IntPoint)
type IntRendering = [IntLine]

renderScalePoint :: Int -> Point -> IntPoint
renderScalePoint f (Point (x, y)) = (round (x * f'), round (y * f')) where f' = toRational f

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled f (Picture lines) = (map roundLine lines) where
  roundLine = (\(p1, p2) -> (renderScalePoint f p1, renderScalePoint f p2))


--- --- Transformation --- ---

data TransformOp = Translation Vec | Rotation R deriving (Show, Eq)
data Transform = Transform [TransformOp] deriving (Eq)

instance Show Transform where
  show (Transform a) = show a


-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = Transform [Translation v]

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate r = Transform [Rotation r]

optimize :: [TransformOp] -> [TransformOp]
optimize ((Translation v1) : (Translation v2) : ts) = optimize $ (Translation (v1 >< v2)) : ts
optimize ((Rotation r1) : (Rotation r2) : ts) = optimize $ (Rotation (r1 + r2)) : ts
optimize (a : b : ts) = a : (optimize $ b : ts)
optimize (a : []) = [a]
optimize [] = []

instance Mon Transform where
  m1 = Transform []
  Transform t1 >< Transform t2 = Transform $ optimize $ t1 ++ t2


fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = toRational 360

sine :: R -> R
sine n
  | 0 <= n && n <= fullCircle / 2 = (4 * n * (180 - n)) / (40500 - (n * (180 - n))) -- Bhaskara formula
  | fullCircle / 2 <= n && n <= fullCircle = -sine (n - (fullCircle / 2))
  | n <= 0 = sine (n + fullCircle)
  | otherwise = sine (n - fullCircle)
--  | otherwise = sine $ n `mod'` fullCircle -- TODO implement better modulo? here 13 mod 3.1 is not 0.6

cosine :: R -> R
cosine n = sine (n + (fullCircle / 4))

c_rotate :: R2 -> R -> R2
c_rotate (x, y) r = ( (x * cs) - (y * sn)
                  , (x * sn) + (y * cs)) where cs = cosine r
                                               sn = sine r
c_translate :: R2 -> R2 -> R2
c_translate (x, y) (xt, yt) = ((x + xt), (y + yt))

trpoint :: Transform -> Point -> Point
trpoint (Transform l) (Point p) = Point $ foldl (\acc h -> case h of
    Translation (Vec t) -> c_translate acc t
    Rotation r -> c_rotate acc r
  ) p l

trvec :: Transform -> Vec -> Vec
trvec (Transform l) (Vec v) = Vec (foldl (\acc h -> case h of
    Translation (Vec t) -> acc
    Rotation r -> c_rotate acc r
  ) v l)

transformLine :: Transform -> PicLine -> PicLine
transformLine t (p1, p2) = (trpoint t p1, trpoint t p2)

transform :: Transform -> Picture -> Picture
transform t (Picture l) = Picture $ map (transformLine t) l


--- --- DEAD CODE STASH --- ---

-- -- Matrix-based version -- --

--data Transform = Transform [[R]]
--
--transpose :: [[R]] -> [[R]]
--transpose ([]:_) = []
--transpose x = (map head x) : transpose (map tail x)
--
--instance Num Transform where
--  (Transform a) + (Transform b) = Transform [ zipWith (+) p q | (p, q) <- zip a b]
--  negate (Transform a) = Transform [[-n | n <- l] | l <- a]
--  (Transform a) * (Transform b) = Transform [[sum $ zipWith (*) ls rs | rs <- transpose b] | ls <- a]
--  abs (Transform _) = undefined -- not used - mocked so there are no warning while implementing rest of Num Transform
--  signum (Transform _) = undefined -- not used - mocked so there are no warning while implementing rest of Num Transform
--  fromInteger _ = undefined -- not used - mocked so there are no warning while implementing rest of Num Transform
--
---- przesunięcie o wektor
--translate :: Vec -> Transform
--translate (Vec (x, y)) = Transform [ [1, 0, x]
--                                   , [0, 1, y]
--                                   , [0, 0, 1]
--                                   ]
--
---- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
---- jednostki mozna sobie wybrać
--rotate :: R -> Transform
--rotate r = Transform [ [cosine(r), -sine(r), 0]
--                     , [sine(r), cosine(r), 0]
--                     , [0, 0, 1] ]
--
--instance Mon Transform where
--  m1 = Transform [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
--  t1 >< t2 = (t1 * t2)

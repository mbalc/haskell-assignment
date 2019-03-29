module Lib where
import Mon
import Data.Fixed(mod')

--- ---  RenderData  --- ---
type R = Rational

type R2 = (R, R)

data Vec = Vec R2     -- wektor 2D
data Point = Point R2 -- punkt 2D

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
data Picture = Picture [PicLine]

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

type IntLine = ((Int, Int), (Int, Int))
type IntRendering = [IntLine]

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled f (Picture lines) = (map roundLine lines) where
  roundLine = (\(Point (a, b), Point (p, q)) -> ((round a, round b), (round p, round q)))


--- --- Transformation --- ---

data TransformOp = Translation Vec | Rotation R
data Transform = [TransformOp]

instance Show TransformOp where
  show Translation v = "Translate " + show a
  show Rotation r = "Rotation " + show r

instance Show Transform where
  show (Transform a) = show a


-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = [Translation v]

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate r = [Rotation R]

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = toRational 360

instance Mon Transform where
m1 = Rotation 0
t1 >< t2 = t1 ++ t2

sine :: R -> R
sine n
  | 0 <= n && n <= fullCircle / 2 = (4 * n * (180 - n)) / (40500 - (n * (180 - n))) -- Bhaskara formula
  | fullCircle / 2 <= n && n <= fullCircle = -sine (n - (fullCircle / 2))
  | otherwise = sine (n `mod'` fullCircle) -- TODO implement better modulo? here 13 mod 3.1 is not 0.6

cosine :: R -> R
cosine n = sine (n - (fullCircle / 4))

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = toRational 360


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

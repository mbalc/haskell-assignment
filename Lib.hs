module Lib where
import Mon

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

data Transform = Transform [[R]]

instance Show Transform where
  show (Transform a) = show a

transpose :: [[R]] -> [[R]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

instance Num Transform where
  (Transform a) + (Transform b) = Transform [ zipWith (+) p q | (p, q) <- zip a b]
  negate (Transform a) = Transform [[-n | n <- l] | l <- a]
  (Transform a) * (Transform b) = Transform [[sum $ zipWith (*) ls rs | rs <- transpose b] | ls <- a]
  abs (Transform _) = 1 -- not used - mocked so there are no warning while implementing rest of Num Transform
  signum (Transform _) = 1 -- not used - mocked so there are no warning while implementing rest of Num Transform
  fromInteger _ = Transform [[]] -- not used - mocked so there are no warning while implementing rest of Num Transform


-- przesunięcie o wektor
translate :: Vec -> Transform
translate (Vec (x, y)) = Transform [ [1, 0, x]
                                   , [0, 1, y]
                                   , [0, 0, 1]
                                   ]


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


data Picture = Picture [(Point, Point)]

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R, R) -> (R, R) -> Picture
line a b = Picture [(point a, point b)]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0, 0)
rectangle :: R -> R -> Picture
rectangle a b = Picture [ (point (0, 0), point(a, 0))
                         , (point (0, 0), point(0, b))
                         , (point (a, 0), point(a, b))
                         , (point (0, b), point(a, b))
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


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

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R, R) -> (R, R) -> Picture
line (x1, y1) (x2, y2) = Picture [Point (x1, y1), Point (x2, y2))]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0, 0)
rectangle :: R -> R -> Picture
rectangle a b = Picture [ line (0, 0) (a, 0)
                        , line (0, 0) (0, b)
                        , line (a, 0) (a, b)
                        , line (0, b) (a, b)
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
(Translate v1) >< (Translate v2) = Translate (v1 >< v2)
(Rotate r1) >< (Rotate r2) = Rotate (r1 + r2)


trvec :: Transform -> Vec -> Vec

trpoint :: TransformOp -> Point -> Point
trpoint (Translation (Vec (xv, yv))) Point (x, y) = Point (x + xv, y + yv)

trpoint :: Transform -> Point -> Point
trpoint (Transform l) (Point x y)  where
  Translate (Vec (xv, yv)) = foldl

transform :: Transform -> Picture -> Picture

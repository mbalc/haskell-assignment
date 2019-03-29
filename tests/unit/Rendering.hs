module Main(main) where
import Lib

main = do
  let v = (toRational 1) / 4
  let p = line (0,0) (v, v)
  let r1 = renderScaled 1 p == [((0, 0), (0, 0))]
  let v = (toRational 3) / 4
  let p = line (0,0) (v, v)
  let r2 = renderScaled 1 p == [((0, 0), (1, 1))]
  let v = (toRational 4) / 4
  let p = line (0,0) (v, v)
  let r3 = renderScaled 1 p == [((0, 0), (1, 1))]
  let v = (toRational 2) / 4
  let p = line (0,0) (v, v)
  let r4 = renderScaled 1 p == [((0, 0), (0, 0))]
  print $ (show (r1, r2, r3, r4))

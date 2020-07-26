import Control.Monad
import Data.List
import Data.Ord

f :: Int -> Int -> Int -> Int -> Int -> [(Int, Int, Int)]
f sap a b c d = do
  x <- [c..d]
  y <- [0..15]
  guard $ if sap < 2 then y < x else y == x
  z <- [0..15]
  guard $ if sap < 3 then z < x else z == x
  guard $ x + y + z `elem` [a .. b]
  return (x, y, z)

extractRange :: [(Int, Int, Int)] -> ((Int, Int), (Int, Int), (Int, Int))
extractRange l = ((mnf s1, mxf s1), (mnf s2, mxf s2), (mnf s3, mxf s3))
  where
    s1 (x, _, _) = x
    s2 (_, x, _) = x
    s3 (_, _, x) = x
    mnf s = minimum $ map s l
    mxf s = maximum $ map s l

order :: [String]
order = map fst $ sortBy (comparing $ extractRange . snd) $ zip
  [ "test_1_amazes_blown"
  , "test_1_amazes_xclnt"
  , "test_1_strong_blown"
  , "test_1_strong_xclnt"
  , "test_1_strong_jbdne"
  , "test_1_decent_blown"
  , "test_1_decent_xclnt"
  , "test_1_decent_jbdne"
  , "test_1_notgrt_blown"
  , "test_1_notgrt_xclnt"
  , "test_1_notgrt_jbdne"
  , "test_1_notgrt_nogrt"
  , "test_2_amazes_blown"
  , "test_2_amazes_xclnt"
  , "test_2_strong_blown"
  , "test_2_strong_xclnt"
  , "test_2_strong_jbdne"
  , "test_2_decent_xclnt"
  , "test_2_decent_jbdne"
  , "test_2_notgrt_jbdne"
  , "test_2_notgrt_nogrt"
  , "test_3_amazes_blown"
  , "test_3_amazes_xclnt"
  , "test_3_strong_jbdne"
  , "test_3_decent_jbdne"
  , "test_3_notgrt_nogrt"
  ]
  [ test_1_amazes_blown
  , test_1_amazes_xclnt
  , test_1_strong_blown
  , test_1_strong_xclnt
  , test_1_strong_jbdne
  , test_1_decent_blown
  , test_1_decent_xclnt
  , test_1_decent_jbdne
  , test_1_notgrt_blown
  , test_1_notgrt_xclnt
  , test_1_notgrt_jbdne
  , test_1_notgrt_nogrt
  , test_2_amazes_blown
  , test_2_amazes_xclnt
  , test_2_strong_blown
  , test_2_strong_xclnt
  , test_2_strong_jbdne
  , test_2_decent_xclnt
  , test_2_decent_jbdne
  , test_2_notgrt_jbdne
  , test_2_notgrt_nogrt
  , test_3_amazes_blown
  , test_3_amazes_xclnt
  , test_3_strong_jbdne
  , test_3_decent_jbdne
  , test_3_notgrt_nogrt
  ]

test_1_amazes_blown = f 1 37 45 15 15 -- ((15,15),(8,14),(8,14))
test_1_amazes_xclnt = f 1 37 45 13 14 -- ((13,14),(10,13),(10,13))
test_1_amazes_jbdne = f 1 37 45  8 12 -- -------------------------
test_1_amazes_nogrt = f 1 37 45  0  7 -- -------------------------
test_1_strong_blown = f 1 30 36 15 15 -- ((15,15),(1,14),(1,14))
test_1_strong_xclnt = f 1 30 36 13 14 -- ((13,14),(3,13),(3,13))
test_1_strong_jbdne = f 1 30 36  8 12 -- ((11,12),(7,11),(7,11))
test_1_strong_nogrt = f 1 30 36  0  7 -- -------------------------
test_1_decent_blown = f 1 23 29 15 15 -- ((15,15),(0,14),(0,14))
test_1_decent_xclnt = f 1 23 29 13 14 -- ((13,14),(0,13),(0,13))
test_1_decent_jbdne = f 1 23 29  8 12 -- ((9,12),(0,11),(0,11))
test_1_decent_nogrt = f 1 23 29  0  7 -- -------------------------
test_1_notgrt_blown = f 1  0 22 15 15 -- ((15,15),(0,7),(0,7))
test_1_notgrt_xclnt = f 1  0 22 13 14 -- ((13,14),(0,9),(0,9))
test_1_notgrt_jbdne = f 1  0 22  8 12 -- ((8,12),(0,10),(0,10))
test_1_notgrt_nogrt = f 1  0 22  0  7 -- ((1,7),(0,6),(0,6))
test_2_amazes_blown = f 2 37 45 15 15 -- ((15,15),(15,15),(7,14))
test_2_amazes_xclnt = f 2 37 45 13 14 -- ((13,14),(13,14),(9,13))
test_2_amazes_jbdne = f 2 37 45  8 12 -- -------------------------
test_2_amazes_nogrt = f 2 37 45  0  7 -- -------------------------
test_2_strong_blown = f 2 30 36 15 15 -- ((15,15),(15,15),(0,6))
test_2_strong_xclnt = f 2 30 36 13 14 -- ((13,14),(13,14),(2,10))
test_2_strong_jbdne = f 2 30 36  8 12 -- ((11,12),(11,12),(6,11))
test_2_strong_nogrt = f 2 30 36  0  7 -- ------------------------
test_2_decent_blown = f 2 23 29 15 15 -- ------------------------
test_2_decent_xclnt = f 2 23 29 13 14 -- ((13,14),(13,14),(0,3))
test_2_decent_jbdne = f 2 23 29  8 12 -- ((8,12),(8,12),(0,9))
test_2_decent_nogrt = f 2 23 29  0  7 -- ------------------------
test_2_notgrt_blown = f 2  0 22 15 15 -- ------------------------
test_2_notgrt_xclnt = f 2  0 22 13 14 -- ------------------------
test_2_notgrt_jbdne = f 2  0 22  8 12 -- ((8,11),(8,11),(0,6))
test_2_notgrt_nogrt = f 2  0 22  0  7 -- ((1,7),(1,7),(0,6))
test_3_amazes_blown = f 3 37 45 15 15 -- ((15,15),(15,15),(15,15))
test_3_amazes_xclnt = f 3 37 45 13 14 -- ((13,14),(13,14),(13,14))
test_3_amazes_jbdne = f 3 37 45  8 12 -- -----------------------
test_3_amazes_nogrt = f 3 37 45  0  7 -- -----------------------
test_3_strong_blown = f 3 30 36 15 15 -- -----------------------
test_3_strong_xclnt = f 3 30 36 13 14 -- -----------------------
test_3_strong_jbdne = f 3 30 36  8 12 -- ((10,12),(10,12),(10,12))
test_3_strong_nogrt = f 3 30 36  0  7 -- -----------------------
test_3_decent_blown = f 3 23 29 15 15 -- -----------------------
test_3_decent_xclnt = f 3 23 29 13 14 -- -----------------------
test_3_decent_jbdne = f 3 23 29  8 12 -- ((8,9),(8,9),(8,9))
test_3_decent_nogrt = f 3 23 29  0  7 -- -----------------------
test_3_notgrt_blown = f 3  0 22 15 15 -- -----------------------
test_3_notgrt_xclnt = f 3  0 22 13 14 -- -----------------------
test_3_notgrt_jbdne = f 3  0 22  8 12 -- -----------------------
test_3_notgrt_nogrt = f 3  0 22  0  7 -- ((0,7),(0,7),(0,7))

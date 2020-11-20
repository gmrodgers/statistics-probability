module DiscreteDistribution
  ( DiscreteDist (..),
    pmf,
    mean,
    variance,
  )
where

data DiscreteDist
  = Uni Int Int
  | Bin Int Float
  | Pois Float
  | Custom (Int -> Float) [Int]

instance Show DiscreteDist where
  show (Uni a b) = "Uni " ++ show a ++ " " ++ show b
  show (Bin n p) = "Bin " ++ show n ++ " " ++ show p
  show (Pois la) = "Pois " ++ show la
  show (Custom _ domain) = "Custom f " ++ show domain

uniformPMF a b _ = 1.0 / fromIntegral (b - a + 1)

binomialPMF n p x = (fromIntegral . combinations n) x * p ^^ x * (1 - p) ^^ (n - x)
  where
    combinations :: Int -> Int -> Int
    combinations n r = div (factorial n) (factorial r * factorial (n - r))

poissonPMF la x
  | x < 0 = 0
  | otherwise = la ^^ x * exp 1 ** (- la) / (fromIntegral . factorial) x

pmf :: DiscreteDist -> Int -> Float
pmf (Uni a b) = uniformPMF a b
pmf (Bin n p) = binomialPMF n p
pmf (Pois la) = poissonPMF la
pmf (Custom f _) = f

mean :: DiscreteDist -> Float
mean (Uni a b) = fromIntegral (a + b) / 2
mean (Bin n p) = fromIntegral n * p
mean (Pois la) = la
mean (Custom f xs) = expectedValue (Custom f xs) id xs

expectedValue :: DiscreteDist -> (Int -> Int) -> [Int] -> Float
expectedValue dist tr = sum . map (\x -> fromIntegral (tr x) * pmf dist x)

variance :: DiscreteDist -> Float
variance (Uni a b) = fromIntegral ((b - a + 1) ^ 2 - 1) / 12
variance (Bin n p) = fromIntegral n * p * (1 - p)
variance (Pois la) = la
variance (Custom f xs) = eX2 - (eX ** 2)
  where
    eX2 = expectedValue (Custom f xs) (^ 2) xs
    eX = expectedValue (Custom f xs) id xs

factorial :: Int -> Int
factorial n = product [1 .. n]
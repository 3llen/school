type Function = Double -> Double

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

xValues = [0.001, 0.002, 0.003, 0.004, 0.005]
sinValues = [0.001, 0.002, 0.003, 0.004, 0.005]
cosValues = [1.0, 0.999998, 0.999996, 0.999992, 0.999988]
cotValues = [1000, 499.999, 333.332, 249.999, 199.998]

lagrange
  :: [(Double, Double)] -- ^ The data points to interpolate
  -> Function -- ^ The function (polynomial)
lagrange ps x = sum $ map s $ enumerate ps where
  s :: (Int, (Double, Double)) -> Double
  s (j, (xj, yj)) = yj * lj where
    lj = product $ map p $ enumerate ps where
      p :: (Int, (Double, Double)) -> Double
      p (m, (xm, _)) =
        if m == j then 1 else (x - xm) / (xj - xm)
-- this is practically a word-for-word translation of the mathematical
-- definition of Lagrange interpolation.

-- We interpolate `cot`.
cotInterp = lagrange (zip xValues cotValues)

-- We interpolate `sin`.
sinInterp = lagrange (zip xValues sinValues)

-- We interpolate `cos`.
cosInterp = lagrange (zip xValues cosValues)

-- | cot(0.0015) computed by interpolating cot from the table.
cotv1 = cotInterp 0.0015
-- = 684.89532815

-- | cot(0.0015) computed by interpolating cos and sin from the table.
cotv2 = cosInterp 0.0015 / sinInterp 0.0015
-- = 666.6658125

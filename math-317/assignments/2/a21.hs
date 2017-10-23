{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Ord (comparing)
import Data.List (intercalate, sortBy, zip6)
import GHC.Exts (IsList(..))

type Function = Double -> Double

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- QUESTION 1

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

-- QUESTION 2

-- | A polynomial is stored as the list of its coefficients in little-endian
-- format, i.e. the first element is the x^0 term, the second is the x^1 term,
-- and so on.
newtype Polynomial = Polynomial { poly :: [Double] }

instance Show Polynomial where
  show (Polynomial cs) = intercalate " + " $ map term (enumerate cs) where
    term (e, c)
      | e == 0 = show c
      | e == 1 = show c ++ " x"
      | otherwise = show c ++ " x^" ++ show e

-- so we can write [1,2,3] for the polynomial 1 + 2x + 3x^2
instance IsList Polynomial where
  type Item Polynomial = Double
  fromList = Polynomial
  toList (Polynomial cs) = cs

-- | Evaluate a polynomial at a point.
eval :: Polynomial -> Function
eval (Polynomial cs) x = sum $ map evalTerm (enumerate cs) where
  evalTerm :: (Int, Double) -> Double
  evalTerm (e, c) = c * x ** (fromIntegral e)

-- | Differentiate a polynomial.
diff :: Polynomial -> Polynomial
diff (Polynomial cs) = Polynomial $ case cs of
  [] -> [] -- derivative of zero is zero
  (_:cs') -> zipWith (*) cs' [1..]

-- | Compute the antiderivative of a polynomial.
-- This selects C = 0.
int :: Polynomial -> Polynomial
int (Polynomial cs) = Polynomial (0 : cs') where
  cs' = zipWith (/) cs [1..] where

-- | A row is just a list of entries.
type Row = [Double]

-- | An augmented matrix is a list of rows reresenting the matrix and a single
-- row (in reality a column) representing the constant that each equation is
-- equal to.
data Matrix = Matrix [Row] Row

-- | Replaces the value at the given index with given value.
-- Returns the old value.
replace :: Int -> a -> [a] -> (a, [a])
replace _ _ [] = error "replace of empty list"
replace n x (y : xs)
  | n < 0 = error "replace at negative index"
  | n == 0 = (y, x : xs)
  | n > 0 = let (z, xs') = replace (n - 1) x xs in (z, y : xs')

-- | Swaps two elements in a list.
swap :: Int -> Int -> [a] -> [a]
swap n m xs
  | n > m = swap m n xs -- smaller index first.
  | n == m = xs -- nothing to do
  | n < m = case (n, xs) of
    (_, []) -> error "swap of empty list"
    (0, x : xs') -> let (y, xs'') = replace (m - 1) x xs' in (y : xs'')
    (n, x : xs') -> x : swap (n-1) (m-1) xs'

-- | Find an element satisfying a predicate in a list.
-- Returns the element and its position.
-- If there is no satisfying element, we get Nothing.
find :: (a -> Bool) -> [a] -> Maybe (a, Int)
find p [] = Nothing
find p (x:xs) =
  if p x
  then pure (x, 0)
  else fmap (+1) <$> find p xs

nonzero :: Double -> Bool
nonzero = (0 /=)

-- | Add rows componentwise.
(|+|) :: Row -> Row -> Row
(|+|) = zipWith (+)
infixl 6 |+|

-- | Multiply a row by a scalar.
(|*|) :: Double -> Row -> Row
(|*|) k = fmap (k *)
infixl 7 |*|

type Equation = (Row, Double)

-- | Given an equation, eliminate it from a matrix by subtracting it the
-- appropriate number of times from all rows in the matrix. This produces a new
-- matrix.
eliminate :: Equation -> Matrix -> Matrix
eliminate _ (Matrix [] []) = Matrix [] []
eliminate (r', c') (Matrix (r:rs) (c:cs)) = Matrix (r'':rs') (c'':cs') where
  p = - head r / head r'
  (c'' : r'') = (c : r) |+| p |*| (c' : r')
  (Matrix rs' cs') = eliminate (r', c') (Matrix rs cs)

-- | Triangularize an augmented matrix.
-- The resulting matrix will be upper triangular, and can be used after to
-- compute the value of each variable.
-- If the matrix is singular, we get back Nothing.
triangularize :: Matrix -> Maybe Matrix
triangularize (Matrix rs cs) = case (rs, cs) of
  (r:rs', c:cs') ->
    let h = head r in
    if nonzero h
    then do
      -- if the head is nonzero, then we divide through by the head to make a
      -- leading 1
      let (c' : r') = (/ h) <$> (c : r)
      -- then we eliminate this row from the later rows
      let (Matrix rs'' cs'') = eliminate (r', c') (Matrix rs' cs')
      -- so the head of every row in rs'' is zero
      -- idea: we drop the head of every row in rs'' and triangularize the
      -- smaller matrix.
      (Matrix rs''' cs''') <- triangularize (Matrix (tail <$> rs'') cs'')
      -- now we put back the zeros, put back the row we've been processing, and
      -- we're done
      pure $ Matrix (r' : ((0:) <$> rs''')) (c' : cs''')
    else do
      -- if r's head is zero, then we have a problem.
      -- We need to find another row later with a nonzero head and swap it to
      -- our current position.
      (r', i) <- find (nonzero . head) rs'
      -- replace r' with r in rs' to get a new list of rows rs'
      let (_, rs'') = replace i r rs'
      -- replace the corresponding constant term
      let (c', cs'') = replace i c cs'
      -- triangularize the augmented matrix now that we've swapped the row and
      -- constant
      triangularize $ Matrix (r':rs'') (c':cs'')
  ([], []) -> pure $ Matrix [] []
  _ -> error "invalid augmented matrix"

showMatrix :: Matrix -> String
showMatrix (Matrix rs cs) = concat $ (++ "\n") . showRow <$> zip rs cs where
  showRow :: Equation -> String
  showRow (r, c) = intercalate " " (show <$> r) ++ " = " ++ show c

showMatrix' :: Maybe Matrix -> IO ()
showMatrix' Nothing = pure ()
showMatrix' (Just m) = putStr (showMatrix m)

-- | Solve an upper triangular matrix by backsubstitution.
-- This is the last step of Gaussian elimination.
backSubstitute :: Matrix -> Row
backSubstitute (Matrix [] []) = []
backSubstitute (Matrix (r:rs) (c:cs)) =
  -- solve the smaller upper triangular matrix obtained from the subsequent
  -- rows and by dropping the zeros in the current column
  let as = backSubstitute (Matrix (tail <$> rs) cs)
  -- `as` is an assignment to variables x_{k+1} to x_n
  -- note that `r` is a row with coefficients for variables x_k to x_n
  -- so next we perform the assignment to the variables and compute
  -- c' = r_{k+1} x_{k+1} + ... + r_n x_n
      c' = sum (zipWith (*) (tail r) as)
  -- finally, the assignment to x_k is (c - c') / r_k
  -- (note r_k will be 1 if the triangular matrix was obtained from
  -- `triangularize` above.)
      in (c - c') / head r : as

-- | Solve an augmented matrix by Gaussian elimination.
-- First, triangularizes the matrix, and then back-substitutes.
-- If the matrix is singular, we get back Nothing.
gaussianSolve :: Matrix -> Maybe Row
gaussianSolve = fmap backSubstitute . triangularize

type Coordinates = [(Double, Double)]

theData :: Coordinates
theData = f <$> xs where
  xs = [0, 0.25, 0.5, 0.75, 1.0]
  f x = (x, sin (pi * x))

-- | From data points, constructs the matrix to solve to get out the fitting
-- parameters for the natural cubic spline interpolation.
naturalCubicSplineMatrix
  :: Coordinates -- ^ coordinates to interpolate
  -> ([Double], Matrix)
  -- ^ the h_j values and the matrix to solve
naturalCubicSplineMatrix cs =
  (hjs, Matrix (r : rs) (head bjs : bs)) where
    r :: Row
    r = (head gammajs) : (head hjs) : replicate k' 0
    (k', Matrix rs bs) =
      go []
        (init $ tail $ tail hjs) -- length: n-4
        (zip3 (tail gammajs) (tail $ tail hjs) (tail bjs))
        --    ^ n-3          ^ n-3             ^ n-3

    xjs = fst <$> cs -- length: n
    fjs = snd <$> cs

    -- the list of deltas in the x positions
    hjs = zipWith (-) (tail xjs) xjs -- length n-1
    -- the list of deltas in the y positions
    dfjs = zipWith (-) (tail fjs) fjs -- length n-1

    -- length: n-2
    gammajs = zipWith g hjs (tail hjs) where
      g hjm1 hj = 2 * (hj + hjm1)

    -- length: n-2
    bjs = zipWith g (zip dfjs hjs) (zip (tail dfjs) (tail hjs)) where
      g (dfjm1, hjm1) (dfj, hj) = 6 * ( (dfj / hj) - (dfjm1 / hjm1) )

    go
      :: Row -- ^ the partial row of zeros that precedes hjm1 gammaj hj
      -> [Double]
      -- ^ h_j
      -> [(Double, Double, Double)]
      -- ^ gamma_j h_{j - 1} b_j
      -> (Int, Matrix)
      -- ^ the number of zeros to put on the end of a new row, the matrix
    go zs [] [(gammaj, hjm1, bj)] = (0, Matrix [zs ++ hjm1 : [gammaj]] [bj])
    go zs (hj:hjs) ((gammaj, hjm1, bj) : qs) =
      case go (0 : zs) hjs qs of
        (k, Matrix rs cs) ->
          ( k + 1
          , Matrix ((zs ++ hjm1 : gammaj : hj : replicate k 0) : rs) (bj : cs)
          )
    go zs hjs gs = error $
      "bogus data " ++ show zs ++ " " ++ show hjs ++ " " ++ show gs

-- construct the cubic spline matrix, solve it, and stick zero for y''_1 and
-- y''_n
-- Now we have all the fitting parameters
cubicSplines :: Coordinates -> Maybe [Polynomial]
cubicSplines (sortBy (comparing fst) -> cs) = do
  let fjs = snd <$> cs
  let xjs = fst <$> cs

  (hjs, y''js) <-
    fmap (fmap ((0.0:) . (++ [0.0])))
    $ sequence $ gaussianSolve
    <$> naturalCubicSplineMatrix cs

  let mkcoeff (fj, y''j, hj) = (fj / hj) - y''j * hj / 6
  let mkgreek y''j hj = y''j / (6 * hj)

  let cjs = mkcoeff <$> zip3 (tail fjs) (tail y''js) hjs
  let djs = mkcoeff <$> zip3 fjs y''js hjs
  let psijs = zipWith mkgreek y''js hjs
  let phijs = zipWith mkgreek (tail y''js) hjs

  let mkpoly (phij, psij, xj1, xj, cj, dj)
        = [ xj1**3 * psij
            + xj**3 * phij
            + dj * xj1
            - cj * xj
          , cj - dj
            + 3 * xj**2 * phij
            - 3 * xj1**2 * psij
          , 3 * xj1 * psij
            - 3 * xj * phij
          , phij - psij
          ]

  pure $ mkpoly <$> zip6 phijs psijs (tail xjs) xjs cjs djs

cubicSpline :: Coordinates -> Maybe Function
cubicSpline cs = do
  ps <- cubicSplines cs
  let f ((p, xj, xj1):pxjs) x =
        if x < xj then 0 else
        if x < xj1 then eval p x else
        f pxjs x
      f [] _ = 0

  pure $ f (zip3 ps (fst <$> cs) (tail $ fst <$> cs))

integrateBySplines :: Coordinates -> Maybe Double
integrateBySplines cs = do
  ps <- fmap int <$> cubicSplines cs
  let i (eval -> f, xj, xj1) = f xj1 - f xj
  pure $ sum $ map i (zip3 ps (fst <$> cs) (tail $ fst <$> cs))

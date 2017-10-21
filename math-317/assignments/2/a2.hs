{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

import Data.List (intercalate)
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
  (Matrix rs' cs''

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
      let (Matrix rs''' cs''') = triangularize (Matrix (tail <$> rs'') cs'')
      -- now we put back the zeros, put back the row we've been processing, and
      -- we're done
      pure $ Matrix (r' : (0:) <$> rs''') (c' : cs''')
    else do
      -- if r's head is zero, then we have a problem.
      -- We need to find another row later with a nonzero head and swap it to
      -- our current position.
      (r', i) <- find nonzero rs'
      -- replace r' with r in rs' to get a new list of rows rs'
      let (_, rs'') = replace i r rs'
      -- replace the corresponding constant term
      let (c', cs'') = replace i c cs'
      -- triangularize the augmented matrix now that we've swapped the row and
      -- constant
      triangularize $ Matrix (r':rs'') (c':cs'')
  ([], []) -> pure []
  _ -> error "invalid augmented matrix"

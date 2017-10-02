import Control.Monad ( forM_ )
import Data.List ( intersperse )
import System.IO

-- | A function takes in a double and produces a double.
type Function = Double -> Double

-- | Performs a single step of the secant method.
secant
  :: Function -- ^ objective function
  -> Double -- ^ first guess
  -> Double -- ^ second guess
  -> Double -- ^ new guess
secant f x0 x1 = x1 - f x1 * m where
  -- m is the slope of the line between f(x0) and f(x1)
  m = (x1 - x0) / (f x1 - f x0)
  -- and the result 'secant' is the zero of the line between
  -- (x0, f(x0)) and (x1, f(x1)).

-- | Performs a step of the secant method, but with a type that's directly
-- amenable to the 'iterate' function.
secant'
  :: Function
  -> (Double, Double) -- ^ (nth guess, (n+1)th guess)
  -> (Double, Double) -- ^ ( (n+1)th guess, (n+2)th guess)
secant' f (x0, x1) = let x2 = secant f x0 x1 in (x1, x2)

-- | Performs a single step of the Newton-Raphson method.
nr
  :: Function -- ^ objective function
  -> Function -- ^ its derivative
  -> Double -- ^ guess
  -> Double -- ^ new guess
nr f df x =
  -- compute the derivative at the guess to get the slope of the tangent
  let a = df x
  -- find the zero of the tangent, this is the new guess
      b = f x - a * x
      in - b / a

-- | The function whose root we want to find.
objective :: Function
objective x = sqrt x - exp (-x)

-- | The derivative of the function whose root we want to find.
objective' :: Function
objective' x = 0.5 / sqrt x + exp (-x)

-- | We recover the full NR method by using the 'iterate' function from the
-- standard library, which repeatedly applies a function to an initial input
-- and collects the list of all the results.
fullNR
  :: Function -- ^ objective function
  -> Function -- ^ its derivative
  -> Double -- ^ the initial guess
  -> [Double] -- ^ infinite list of all intermediate steps
fullNR f df = iterate (nr f df)

-- | Same idea. We use the 'iterate' library function on our function secant'
-- to get a list of /pairs/ of guesses. Note that by the definition of secant',
-- the list of tuples will look like this:
--   (x0, x1), (x1, x2), (x2, x3), etc.
-- so to remove the duplicates, we take the first component of every pair in
-- the list.
fullSecant
  :: Function -- ^ objective function
  -> Double -- ^ first guess
  -> Double -- ^ second guess
  -> [Double] -- ^ infinite list of all intermediate steps
fullSecant f x0 x1 = map fst (iterate (secant' f) (x0, x1))

type Coordinates = [(Double, Double)]

-- | For convenience we define a function to analyze the results of a method.
-- It takes the first n values from the infinite list of doubles passed as
-- input, and it applies the given function to each, to produce a list of
-- coordinates. This lets us see how quickly a method converges.
analyze :: Int -> Function -> [Double] -> Coordinates
analyze n f = map g . take n where
  g x = (x, f x)

-- | The results of the secant method, for a bunch of guesses.
secantResults :: [((Double, Double), Coordinates)]
secantResults =
  let f x0 x1 = ( (x0, x1), analyze 20 objective (fullSecant objective x0 x1) ) in
  [ f 0 1
  , f 1 2
  , f 0 2
  , f 1 3
  , f 2 4
  ]

myGuesses :: [Double]
myGuesses = [0, 0.25, 0.5, 0.75, 1, 2, 5]

-- | The results of the Newton-Raphson method, for a bunch of guesses.
nrResults :: [(Double, Coordinates)]
nrResults =
  let f x0 = (x0, analyze 20 objective (fullNR objective objective' x0)) in
  map f myGuesses

-- Now some fixed-point iteration schemes.

fix1 :: Double -> Double
fix1 x = exp (-2 * x)

fix2 :: Double -> Double
fix2 x = -1/2 * log x

fix1results :: [(Double, Coordinates)]
fix1results =
  let f x0 = (x0, analyze 50 objective (iterate fix1 x0)) in
  map f myGuesses

fix2results :: [(Double, Coordinates)]
fix2results =
  let f x0 = (x0, analyze 15 objective (iterate fix2 x0)) in
  map f myGuesses

-- | For problem 5.
fixl
  :: Double -- ^ parameter lambda
  -> Double
  -> Double
fixl l x = n / d where
  n = l * x + 1 - sin x
  d = 1 + l

fixlresults :: Double -> [(Double, Coordinates)]
fixlresults l =
  let g x = 1 - x - sin x
      f x0 = (x0, analyze 30 g (iterate (fixl l) x0))
      in map f myGuesses

-- | Print out the results to files.
main :: IO ()
main = do
  -- we have to do the secant results separately because their header is
  -- different.
  results2file "secant-results.txt" secantResults
  mapM_ (uncurry results2file)
    [ ("nr-results.txt", nrResults)
    , ("fix1-results.txt", fix1results)
    , ("fix2-results.txt", fix2results)
    , ("fixl-results.txt", fixlresults 1)
    ]

results2file :: Show a => String -> [(a, Coordinates)] -> IO ()
results2file name dat = withFile name WriteMode (putResults dat)

putResults :: Show a => [(a, Coordinates)] -> Handle -> IO ()
putResults results h =
  sequence_ $
  intersperse (hPutStrLn h . replicate 40 $ '-') $
  map (\x -> putResult x h) results

putResult :: Show a => (a, Coordinates) -> Handle -> IO ()
putResult (guess, coords) h = do
  hPutStrLn h $ "initial guess: " ++ show guess
  hPutStrLn h $ "x value" ++ "\t\t\t" ++ "y value"
  forM_ coords $ \(x, y) -> do
    hPutStrLn h $ show x ++ "\t\t\t" ++ show y

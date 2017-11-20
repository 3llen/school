{-# LANGUAGE ViewPatterns #-}

import Data.List ( intercalate )
import Debug.Trace

l = 9 -- meters
dt = 0.1 -- seconds
v0 = 0.6 -- meters / second
theta0 = 0 -- radians
g = 9.8 -- meters / second^2
tmax = 25 -- seconds
epsilon = 0.001 -- tolerance for Newton-Raphson convergence
iterMax = 2000 -- max iterations of Newton-Raphson to do for backwards Euler

-- | This type describes a family of ODEs.
-- Specifically, it represents a function for the form
-- > f(t, v) = dv/dt
-- where @t@ is the independent variable, @v@ is a vector of dependent
-- parameters and @dv/dt@ is a vector of derivatives wrt @t@.
type ODE = Double -> Params -> Params
type Params = [Double]

theOde :: ODE
theOde t [theta, v] = [v / l, - g * sin theta]

-- | Pointwise addition.
(<+>) :: Num a => [a] -> [a] -> [a]
(<+>) = zipWith (+)
infixl 6 <+>

-- | Scalar multiplication.
(*|) k = ((k*) <$>)
infixl 7 *|

------ IMPLEMENTATION OF FORWARD EULER ------

-- | Perform a step of forward Euler on an ODE.
forwardEulerStep :: ODE -> (Double, Params) -> (Double, Params)
forwardEulerStep ode (t, ps) = (t + dt, ps <+> dt *| ode t ps)
  -- The new value for the independent variable is t + dt.
  -- For each equation, the value at the next time step is obtained by taking
  -- its previous value (in the vector ps) and adding the rectangle for the
  -- current time step.

-- | Run a forward Euler simluation (forever).
--
-- This function computes an infinite list of data points.
-- In our output, we take values from it provided that the independent variable
-- is no greater than 140.
forwardEuler
  :: ODE -- ^ the ODE to solve
  -> (Double, Params) -- ^ the initial conditions
  -> [(Double, Params)] -- ^ the solution
forwardEuler ode = iterate (forwardEulerStep ode)

------ IMPLEMENTATION OF BACKWARD EULER ------

type Function = Double -> Double

-- To do the Backward Euler method, we need to do root-finding as an
-- intermediate procedure. We chose the Second method for its reasonable
-- convergence and since we can always bracket the root between 0 and 2pi.

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

-- | Performs one step of backwards Euler. This function is not nice and
-- generic like the forwards Euler implementation, since we require special
-- handling due to the implicit nature of the scheme.
backwardEulerStep :: (Double, Params) -> (Double, Params)
backwardEulerStep (t, [theta, v]) = (t + dt, [theta', v']) where
  v' = v - g * sin theta' * dt
  theta' = converge epsilon iterMax $ fullSecant f 0 (2*pi - epsilon)

  f x = (l/dt) * (x - theta) + g * dt * sin x - v

fmod :: Double -> Double -> Double
fmod m r = r - fromIntegral (floor $ r/m) * m

-- | Determines when a list of numbers converges by using a tolerance and a
-- maximum number of iterations.
converge :: Double -> Int -> [Double] -> Double
converge tol n (takeWhile (not . isNaN) -> xs) = case dropWhile ((> tol) . snd) ds of
  -- We drop leading elements in the list of pairs whose difference with the
  -- previous element is above the tolerance
  -- If the list that results from this dropping is empty, it's that the method
  -- failed to converge in the given number of iterations `n`.
  [] -> error "failed to converge"
  -- Otherwise, we have convergence, and `x` here is the first value whose
  -- difference with the preceding element is below the tolerance.
  (x:_) -> fst x
  where
    ds :: [(Double, Double)]
    ds = take n (tail xs) `zip` (tail xs <+> (-1) *| xs)
    -- the list of differences in the stream paired with the stream
    -- so each element in this list of pairs knows what its difference with the
    -- previous element is

-- | Repeatedly performs steps of backward Euler given an initial
-- configuration.
backwardEuler :: (Double, Params) -> [(Double, Params)]
backwardEuler = iterate backwardEulerStep

------ OUTPUT ------

showRow :: (Double, Params) -> String
showRow (t, ps) = intercalate " " (map show (t : ps))

main :: IO ()
main = do
  let sim = takeWhile ((<= tmax) . fst)
  let initial = (0, [theta0, v0])
  let newline = (++ "\n")
  let header = ("t theta v\n" ++)
  let writeOutput name = writeFile name . header . concatMap (newline . showRow)

  let feSol = sim $ forwardEuler theOde initial
  let beSol = sim $ backwardEuler initial

  writeOutput "output/fe_ode2.txt" feSol
  writeOutput "output/be_ode2.txt" beSol

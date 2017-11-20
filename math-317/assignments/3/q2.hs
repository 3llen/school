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
theOde t [v, theta] = [- g * sin theta, v / l ]

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
-- intermediate procedure. We chose the Newton-Raphson method for its fast
-- convergence and since the functions we're dealing with have easily
-- computable derivatives.

-- | Performs a single step of the Newton-Raphson method.
nrStep
  :: Function -- ^ objective function
  -> Function -- ^ its derivative
  -> Double -- ^ guess
  -> Double -- ^ new guess
nrStep f df x =
  -- compute the derivative at the guess to get the slope of the tangent
  let a = df x
  -- find the zero of the tangent, this is the new guess
      b = f x - a * x
      in - b / a

-- | We recover the full NR method by using the 'iterate' function from the
-- standard library, which repeatedly applies a function to an initial input
-- and collects the infinite list of all the results.
nr
  :: Function -- ^ objective function
  -> Function -- ^ its derivative
  -> Double -- ^ the initial guess
  -> [Double] -- ^ infinite list of all intermediate steps
nr f df = iterate (nrStep f df)

-- | Performs one step of backwards Euler. This function is not nice and
-- generic like the forwards Euler implementation, since we require special
-- handling due to the implicit nature of the scheme.
backwardEulerStep :: (Double, Params) -> (Double, Params)
backwardEulerStep (t, [theta, v]) = (t + dt, [theta', v']) where
  v' = (l/dt) * (theta' - theta)
  theta' = converge epsilon iterMax $ nr f f' theta

  f x = ((l/dt) * (x - theta) - v) / dt + g * sin x
  f' x = (l/dt**2) * x + g * cos x

traceShowing x = traceShow x x

-- | Determines when a list of numbers converges by using a tolerance and a
-- maximum number of iterations.
converge :: Double -> Int -> [Double] -> Double
converge tol n xs = case dropWhile ((> tol) . snd) ds of
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
    ds = traceShowing (take n (tail xs)) `zip` (tail xs <+> (-1) *| xs)
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
  let initial = (0, [v0, theta0])
  let newline = (++ "\n")
  let writeOutput name = writeFile name . concatMap (newline . showRow)

  let feSol = sim $ forwardEuler theOde initial
  let beSol = sim $ backwardEuler initial

  writeOutput "output/fe_ode2.txt" feSol
  writeOutput "output/be_ode2.txt" beSol

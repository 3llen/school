import Data.List ( intercalate )

x0 = 1
y0 = -1
z0 = 30

sigma = 10
beta = 8/3
rho = 28

dt = 0.005
tmax = 100

type ODE = Double -> Params -> Params
type Params = [Double]

theOde :: ODE
theOde t [x, y, z] = [sigma * (y - x), x * (rho - z) - y, x * y - beta * z]

-- | Pointwise addition.
(<+>) = zipWith (+)
infixl 6 <+>

-- | Scalar multiplication.
(*|) k = ((k*) <$>)
infixl 7 *|

------ IMPLEMENTATION OF IMPROVED EULER ------

-- | Perform a step of forward Euler on an ODE.
forwardEulerStep :: ODE -> (Double, Params) -> (Double, Params)
forwardEulerStep ode (t, ps) = (t + dt, ps <+> dt *| ode t ps)
  -- The new value for the independent variable is t + dt.
  -- For each equation, the value at the next time step is obtained by taking
  -- its previous value (in the vector ps) and adding the rectangle for the
  -- current time step.

-- | Perform a step of improved Euler on an ODE.
improvedEulerStep :: ODE -> (Double, Params) -> (Double, Params)
improvedEulerStep ode (t, ps) = (t', ps'') where
  -- compute the next step using forward Euler
  (t', ps') = forwardEulerStep ode (t, ps)

  -- use the next step calculated this way to compute the area of the trapezoid
  ps'' = ps <+> (dt/2) *| (ode t ps <+> ode t' ps')

-- | Run a forward Euler simluation (forever).
--
-- This function computes an infinite list of data points.
-- In our output, we take values from it provided that the independent variable
-- is no greater than 140.
improvedEuler
  :: ODE -- ^ the ODE to solve
  -> (Double, Params) -- ^ the initial conditions
  -> [(Double, Params)] -- ^ the solution
improvedEuler ode = iterate (improvedEulerStep ode)

------ OUTPUT ------

showRow :: (Double, Params) -> String
showRow (t, ps) = intercalate " " (map show (t : ps))

main :: IO ()
main = do
  let sim = take 2000 . takeWhile ((<= tmax) . fst)
  let initial = (0, [x0, y0, z0])
  let newline = (++ "\n")
  let header = ("t x y z\n" ++)
  let writeOutput name = writeFile name . header . (concatMap (newline . showRow))
  let sol = sim $ improvedEuler theOde initial
  writeOutput "output/ie_ode3.txt" sol

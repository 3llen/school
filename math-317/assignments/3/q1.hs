import Data.List ( intercalate )

y0 = 0 -- km
x0 = 600 -- km
dt = 0.5 -- hours
period = 72 -- hours
tmax = 140 -- hours

omega = 2 * pi / period

-- | This type describes a family of ODEs.
-- Specifically, it represents a function for the form
-- > f(t, v) = dv/dt
-- where @t@ is the independent variable, @v@ is a vector of dependent
-- parameters and @dv/dt@ is a vector of derivatives wrt @t@.
type ODE = Double -> Params -> Params
type Params = [Double]

theOde :: ODE
theOde t [x, y] = [- omega * y, omega * x]

-- | Pointwise addition.
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

------ IMPLEMENTATION OF CENTERED DIFFERENCES ------

-- | Perform a step of centered differences on an ODE.
centeredDifferenceStep :: ODE -> (Double, (Params, Params)) -> (Double, (Params, Params))
centeredDifferenceStep ode (t, (vprev, vcur)) =
  (t + dt, (vcur, 2 * dt *| ode t vcur <+> vprev))
  -- The new time is at t + dt.
  -- The previous values for the next iteration are the current interation's
  -- current values.

centeredDifferences
  :: ODE -- ^ the ODE to solve
  -> (Double, Params) -- ^ the initial conditions
  -> [(Double, Params)] -- ^ the solution
centeredDifferences ode (t0, p0) =
  (t0, p0) : map (fmap snd) sim where
    (t1, p1) = forwardEulerStep ode (t0, p0)
    sim = iterate (centeredDifferenceStep ode) (t1, (p0, p1))
  -- why this crazy map business after iterate?
  -- `centeredDifferenceStep` outputs a *pair* of params along with a time
  -- tacked on the front, so we traverse the list constructed by `iterate` and
  -- keep only the second Params

------ OUTPUT ------

showRow :: (Double, Params) -> String
showRow (t, ps) = intercalate " " (map show (t : ps))

main :: IO ()
main = do
  let sim140h = takeWhile ((<= tmax) . fst)
  let initial = (0, [x0, y0])
  let newline = (++ "\n")
  let header = ("t x y\n" ++)
  let writeOutput name = writeFile name . header . (concatMap (newline . showRow))

  let feSol = sim140h $ forwardEuler theOde initial
  writeOutput "output/fe_ode1.txt" feSol

  let cdSol = sim140h $ centeredDifferences theOde initial
  writeOutput "output/cd_ode1.txt" cdSol

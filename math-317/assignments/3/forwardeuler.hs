import Data.List ( intercalate )

y0 = 0 -- km
x0 = 600 -- km
dt = 0.5 -- hours
period = 72 -- hours

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
forwardEuler
  :: ODE -- ^ the ODE to solve
  -> (Double, Params) -- ^ the initial conditions
  -> [(Double, Params)] -- ^ the solution
forwardEuler ode = iterate (forwardEulerStep ode)

------ IMPLEMENTATION OF CENTERED DIFFERENCES ------


------ OUTPUT ------

showRow :: (Double, Params) -> String
showRow (t, ps) = intercalate " " (map show (t : ps))

main :: IO ()
main = do
  let sol = takeWhile ((<=140) . fst) $ forwardEuler theOde (0, [x0, y0])
  writeFile "output/fe_ode1.txt" (map showRow sol)

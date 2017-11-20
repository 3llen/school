import Control.Monad ( forM_ )
import Data.List ( intercalate )

tmax = pi

type Params = [Double]

------ IMPLEMENTATION OF BACKWARD EULER ------

backwardEulerStep :: Double -> (Double, Params) -> (Double, Params)
backwardEulerStep h (t, [u1, u2]) = (t + h, [u1', u2']) where
  u2' = (u2 - 9 * h * u1) / (1 + 2 * h + 9 * h ** 2)
  u1' = u1 + u2' * h
  -- u1' = (u1 + u2 * h) / (3 + 9 * h)
  -- u2' = (u2 - 9 * u1') / 3

backwardEuler :: Double -> (Double, Params) -> [(Double, Params)]
backwardEuler h = iterate (backwardEulerStep h)

------ ANALYTICAL SOLUTION ------

y t = sqrt 2 / 4 * exp (-t) * sin (2 * sqrt 2 * t)

------ OUTPUT ------

showRow :: (Double, Params) -> String
showRow (t, ps) = intercalate " " (map show (t : ps))

main :: IO ()
main = do
  let initial = (0, [0, 1])
  let sim = takeWhile ((<= tmax) . fst)
  let sols = (\h -> sim $ backwardEuler h initial) <$> [0.001, 0.05, 0.1]

  let newline = (++ "\n")
  let header = ("t u1 u2\n" ++)
  let writeOutput name = writeFile name . header . (concatMap (newline . showRow))

  forM_ (zip ["small", "med", "large"] sols) $ \(name, sol) -> do
    writeOutput ("output/ode3-" ++ name ++ ".txt") sol

  let xs = fst <$> head sols
  let analyticalSol = zip xs ((pure . y) <$> xs)
  writeOutput "output/ode3-analytical.txt" analyticalSol

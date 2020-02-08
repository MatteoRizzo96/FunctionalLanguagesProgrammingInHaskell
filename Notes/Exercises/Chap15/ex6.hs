module Ex6 where

calculateIterations :: Double -> Double -> [(Double, Double)]
calculateIterations s n = [(x, y) | (x, y) <- zip (tail iters) iters] where iters = iterate next s
                                                                            next a = (a + n / a) / 2

sqroot :: Double -> Double
sqroot n = snd (last (takeWhile f iterations)) where f (x, y) = abs (x - y) > 1.0e-5
                                                     iterations = calculateIterations 0.1 ns